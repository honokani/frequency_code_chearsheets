# common modules
import os
import time
# piped modules
import cv2
import h5py
import numpy as np
# keras
import keras.backend as K
from keras.utils import generic_utils as k_utl
from keras.models                      import Model, Sequential
from keras.layers                      import Input, Dense, Activation, Concatenate
from keras.layers                      import Dropout, Flatten, Reshape
from keras.layers                      import Conv2D, UpSampling2D
from keras.layers.normalization        import BatchNormalization
from keras.layers.advanced_activations import LeakyReLU
from keras.optimizers                  import Adam, SGD


DATA_DIR = "data"
INPUT_DIR = "hdf5"
INPUT_DIR_PATH = os.path.join(DATA_DIR, INPUT_DIR)
HDF5_PATH = os.path.join(INPUT_DIR_PATH, "dataset.hdf5")


def normalize_x(x):
    return x / 127.5 - 1


def denormalize_x(x):
    return (x + 1) * 127.5


def load_data(data_p):
    with h5py.File(data_p, "r") as hf:
        X_t_og = hf["train_data_original"][:].astype(np.float32)
        X_t_gn = hf["train_data_genuin"][:].astype(np.float32)
        X_v_og = hf["valid_data_original"][:].astype(np.float32)
        X_v_gn = hf["valid_data_genuin"][:].astype(np.float32)
        X_t_og = normalize_x(X_t_og)
        X_t_gn = normalize_x(X_t_gn)
        X_v_og = normalize_x(X_v_og)
        X_v_gn = normalize_x(X_v_gn)
        return X_t_og, X_t_gn, X_v_og, X_v_gn


def unet_with_upsample(img_shape, model_name="unet_with_upsampling"):
    upcv_shift_infos = []
    min_size = min(img_shape[:-1])
    conv_depth = int(np.ceil(np.log(min_size)/np.log(2)))

    # input
    input_name = "unet_input"
    unet_input = Input( shape=img_shape, name=input_name )
    x = unet_input

    # prepare for conv
    def get_cv_name(i):
        return "unet_conv2D_{}".format(i+1)
    cv_win = (3,3)
    cv_srd = (2,2)
    cv_pad = "same"
    cv_chan = [ min(512, 64*(2**x)) for x in range(conv_depth)] # like [64, 126, 256, ...]
    # convolution2d
    for n in range(conv_depth):
        cv_lay = Conv2D( cv_chan[n], cv_win, strides=cv_srd, padding=cv_pad, name=get_cv_name(n) )
        if n == 0:
            x = cv_lay(x)
        else:
            x = LeakyReLU( 0.2 )(x)
            x = cv_lay(x)
            x = BatchNormalization( axis=-1 )(x)
        if n < conv_depth - 1:
            upcv_shift_infos.append(x)

    # prepare for upsmpl
    def get_upsm_name(i):
        return "unet_up_Upsmpl_{}".format(i+1)
    def get_upcv_name(i):
        return "unet_up_Conv2D_{}".format(i+1)
    upcv_win = cv_win
    upcv_srd = cv_srd
    upcv_pad = cv_pad
    upcv_chan = cv_chan[::-1][1:] # like [..., 256, 128, 64]
    # upsampling2d
    for n in range(conv_depth - 1):
        x = Activation( 'relu' )(x)
        x = UpSampling2D( size=upcv_srd, name=get_upsm_name(n) )(x)
        x = Conv2D( upcv_chan[n], upcv_win, padding=upcv_pad, name=get_upcv_name(n) )(x)
        x = BatchNormalization( axis=-1 )(x)
        if n < 3:
            x = Dropout( 0.5 )(x)
        x = Concatenate( axis=-1 )( [x, upcv_shift_infos[-(n+1)]] )

    # finish to adjust genuin_imgs
    x = Activation( 'relu' )(x)
    x = UpSampling2D( size=upcv_srd )(x)
    x = Conv2D( img_shape[-1], upcv_win, name="last_conv", padding=upcv_pad )(x)
    x = Activation( 'tanh' )(x)

    return Model( inputs=[unet_input], outputs=[x], name=model_name)



def dcgan_discriminate_all(patch_num, img_shape, model_name='DCGAN_discriminate_all'):
    # prepare inputs
    def get_og_name(i):
        return "discri_inputs_{}_og".format(i+1)
    def get_sp_name(i):
        return "discri_inputs_{}_sp".format(i+1)

    # inputs
    og_inputs = []
    sp_inputs = []
    for i in range(patch_num):
        og_inputs.append( Input(shape=img_shape, name=get_og_name(i)) )
        sp_inputs.append( Input(shape=img_shape, name=get_sp_name(i)) )

    # apply discrimination for each inputs
    apply_2_discriminator = dcgan_discriminator(img_shape)
    x = [apply_2_discriminator([oi, si]) for oi, si in zip(og_inputs, sp_inputs)]

    x = Concatenate( axis=-1 )(x)
    x = Dense( 2, activation='softmax', name='discri_output' )(x)

    return  Model( inputs=(og_inputs+sp_inputs), outputs=[x], name=model_name )



def dcgan_discriminator(img_shape, model_name='DCGAN_discriminator'):
    min_size = min(img_shape[:-1])
    conv_depth = int(np.ceil(np.log(min_size)/np.log(2)))
    conv_depth += 1 # Add 1 caused by concatination

    # input
    og_input_name = "dcgan_original_input"
    og_input = Input( shape=img_shape, name=og_input_name )
    sp_input_name = "dcgan_suspicios_input"
    sp_input = Input( shape=img_shape, name=sp_input_name )

    x_og = og_input
    x_sp = sp_input

    # prepare for conv
    def get_cv_name(i):
        return "discri_conv2D_{}".format(i+1)
    cv_win = (3,3)
    cv_srd = (2,2)
    cv_pad = "same"
    cv_chan = [ min(512, 64*(2**x)) for x in range(conv_depth)] # like [64, 126, 256, ...]
    i = 0
    # first original-side Conv
    x_og = Conv2D( cv_chan[i], cv_win, strides=cv_srd, name=get_cv_name(i)+"_og", padding=cv_pad )(x_og)
    x_og = BatchNormalization( axis=-1 )(x_og)
    x_og = LeakyReLU( 0.2 )(x_og)
    # first suspicious-side Conv
    x_sp = Conv2D( cv_chan[i], cv_win, strides=cv_srd, name=get_cv_name(i)+"_sp", padding=cv_pad )(x_sp)
    x_sp = BatchNormalization( axis=-1 )(x_sp)
    x_sp = LeakyReLU( 0.2 )(x_sp)

    # concatinate original and suspicious
    i += 1
    x = Concatenate( axis=-1 )( [x_og, x_sp] )
    # convolution2d to (1x1xN)
    i += 1
    for n in range(i, conv_depth):
        x = Conv2D( cv_chan[n], cv_win, strides=cv_srd, name=get_cv_name(n), padding=cv_pad)(x)
        x = BatchNormalization( axis=-1 )(x)
        x = LeakyReLU( 0.2 )(x)

    # flatten to (N) and dence
    x = Flatten()(x)
    x = Dense( 2, activation='softmax', name='discri_Dense' )(x)

    return Model( inputs=[og_input, sp_input], outputs=[x], name=model_name)


def patch_dcgan(img_shape, generator, discriminator, patch_size, model_name="PATCH_DCGAN"):
    pass


def create_dcgan(img_shape, generator, discriminator, model_name="DCGAN"):
    input_img = Input(shape=img_shape, name='DCGAN_input')
    genarated_img = generator(input_img)
    output_judge = discriminator([input_img, genarated_img])
    return Model( inputs=[input_img], outputs=[genarated_img, output_judge], name=model_name )


def l1_loss(y_true, y_pred):
    return K.sum(K.abs(y_pred - y_true), axis=-1)


def train(X_t_og, X_t_gn, X_v_og, X_v_gn, training_times):
    img_shape = X_v_og.shape[1:]
    epoc_sz, bat_elem_sz = training_times
    t_num = X_t_og.shape[0]
    v_num = X_v_og.shape[0]
    bat_loop_sz = t_num // bat_elem_sz

    m_generator = unet_with_upsample(img_shape)
    m_discriminator = dcgan_discriminator(img_shape)

    # build DCGAN
    opt_dcgan = Adam(lr=1E-3, beta_1=0.9, beta_2=0.999, epsilon=1e-08)
    loss = [l1_loss, 'binary_crossentropy']
    loss_w = [1E1, 1]
    m_discriminator.trainable = False
    m_dcgan = create_dcgan(img_shape, m_generator, m_discriminator)
    m_dcgan.compile(loss=loss, loss_weights=loss_w, optimizer=opt_dcgan)

    # create Generator and Discriminator
    opt_common = Adam(lr=1E-3, beta_1=0.9, beta_2=0.999, epsilon=1e-08)
    m_generator.compile(loss='mae', optimizer=opt_common)
    m_discriminator.trainable = True
    m_discriminator.compile(loss='binary_crossentropy', optimizer=opt_common)

    for e in range(epoc_sz):
        starttime = time.time()

        perm = np.random.permutation(X_t_og.shape[0])
        X_t_og_shuffled = X_t_og[perm]
        X_t_gn_shuffled = X_t_gn[perm]
        X_t_og_splitten = [X_t_og_shuffled[i*bat_elem_sz:(i+1)*bat_elem_sz] for i in range(bat_loop_sz)]
        print(len(X_t_og_splitten))
        X_t_gn_splitten = [X_t_gn_shuffled[i*bat_elem_sz:(i+1)*bat_elem_sz] for i in range(bat_loop_sz)]

        progbar = k_utl.Progbar(t_num*bat_elem_sz)
        for b_idx, (x_org, x_gen) in zip(range(bat_loop_sz), zip(X_t_og_splitten, X_t_gn_splitten)):
            # Unfreeze the discriminator
            m_discriminator.trainable = True
            x_org, x_susp, ans = get_tricked_x_and_answer(x_org, x_gen, m_generator, b_idx, bat_elem_sz)
            discri_loss = m_discriminator.train_on_batch([x_org, x_susp], ans)

            # Freeze the discriminator
            m_discriminator.trainable = False
            challange_idxs = np.random.choice(t_num, bat_elem_sz)
            x_challange_og, x_challange_gn = X_t_og[challange_idxs], X_t_gn[challange_idxs]
            ans_challange = np.zeros((bat_elem_sz, 2), dtype=np.uint8)
            ans_challange[:, 1] = 1
            challange_loss = m_dcgan.train_on_batch(x_challange_gn, [x_challange_og, ans_challange])

            progbar.add( bat_loop_sz
                       , values=[ ("D logloss", discri_loss)
                                , ("G tot"    , challange_loss[0])
                                , ("G L1"     , challange_loss[1])
                                , ("G logloss", challange_loss[2])
                                ]
                       )
            if b_idx % 100 == 0:
                save_idxs = np.random.choice(X_v_og.shape[0], 24)
                x_sv_og, x_sv_gn = X_v_og[save_idxs], X_v_gn[save_idxs]
                save_image(x_sv_og, x_sv_gn, m_generator, "{}_{}".format(str(e), str(b_idx)))
        print("")
        print('Epoch %s/%s, Time: %s' % (e + 1, epoc_sz, time.time() - starttime))


def save_image(x_og, x_gn, m_generator, name):
    fakes = m_generator.predict(x_og)
    sv = None
    for i, i_og in enumerate(x_og):
        im1 = denormalize_x(i_og)
        im2 = denormalize_x(fakes[i])
        im12 = cv2.vconcat([im1, im2])
        im3 = denormalize_x(x_gn[i])
        im123 = cv2.vconcat([im12, im3])
        if sv is None:
            sv = im123
        else:
            sv = cv2.hconcat([sv, im123])
    sv = sv * 255
    sv = sv.astype(np.uint8)
    cv2.imwrite("output/{}.png".format(name), sv)


def get_tricked_x_and_answer(x_org, x_gen, m_generator, b_idx, bat_elem_sz):
    answer = np.zeros((bat_elem_sz, 2), dtype=np.uint8)
    if b_idx % 2 == 0:
        x_fake = m_generator.predict(x_org)
        answer[:, 0] = 1
        return x_org, x_fake, answer
    else:
        answer[:, 1] = 1
        return x_org, x_gen, answer


def main():
    # load data
    X_t_og, X_t_gn, X_v_og, X_v_gn = load_data(HDF5_PATH)
    if X_v_og.shape[0:] == X_v_gn.shape[0:]:
        epoc_sz, batch_sz = 10, 60
        train(X_t_og, X_t_gn, X_v_og, X_v_gn, (epoc_sz, batch_sz))


main()


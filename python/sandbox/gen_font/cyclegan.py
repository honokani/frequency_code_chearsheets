# common modules
import os
import time
from enum import Enum, auto
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
OUTPUT_DIR = "./output"
MODEL_DIR = "model"
PIC_DIR = "pics"
MODEL_DIR_PATH = os.path.join(OUTPUT_DIR, MODEL_DIR)
PIC_DIR_PATH = os.path.join(OUTPUT_DIR, PIC_DIR)


def main():
    X_t_A, X_t_B, X_v_A, X_v_B = load_data(HDF5_PATH)
    if X_v_A.shape[0:] == X_v_B.shape[0:]:
        cgan = CycleGan(X_v_A.shape[1:], X_v_B.shape[1:])
        epoc_sz, batch_sz = 200, 60
        cgan.train(X_t_A, X_t_B, X_v_A, X_v_B, (epoc_sz, batch_sz))


def load_data(data_p):
    with h5py.File(data_p, "r") as hf:
        X_t_A = hf["train_data_original"][:].astype(np.float32)
        X_t_B = hf["train_data_genuin"][:].astype(np.float32)
        X_v_A = hf["valid_data_original"][:].astype(np.float32)
        X_v_B = hf["valid_data_genuin"][:].astype(np.float32)
        X_t_A = normalize_x(X_t_A)
        X_t_B = normalize_x(X_t_B)
        X_v_A = normalize_x(X_v_A)
        X_v_B = normalize_x(X_v_B)
        return X_t_A, X_t_B, X_v_A, X_v_B


def normalize_x(x):
    return x * 2. - 1.


def denormalize_x(x):
    return (x + 1) / 2.





class GenKind(Enum):
    UNET_UPSAMPLE = ("unet_with_upsample")
    def __new__(cls, *args):
        obj = object.__new__(cls)
        n = len(cls.__members__) + 1
        obj._idx = n
        return obj
    def __init__(self, name):
        self._name = name
        self._base = ""
    def set_base(self, base, in_sh, out_sh):
        self._base = "{}_{}".format(base, self._name)
        self.in_sh = in_sh
        self.out_sh = out_sh
        min_size = min(in_sh[:-1])
        self.depth = int(np.ceil(np.log(min_size)/np.log(2)))
        return self
    def basename(self):
        return self._base
    def layername(self, work=""):
        return "{}_{}".format(self._base, work)


class CycleGan():
    def __init__(self, sh_A, sh_B):
        self.sh_A = sh_A
        self.sh_B = sh_B
        self.gen_AB_net = GenKind.UNET_UPSAMPLE.set_base("genAB", sh_A, sh_B)
        self.gen_BA_net = GenKind.UNET_UPSAMPLE.set_base("genBA", sh_B, sh_A)
        self.build_cycle_gan()

    def build_cycle_gan(self):
        self.m_genAB = self.get_generator(self.gen_AB_net)
        self.m_genBA = self.get_generator(self.gen_BA_net)
        self.m_discriA = self.get_discriminator(self.sh_A, "discriA")
        self.m_discriB = self.get_discriminator(self.sh_B, "discriB")

        opt_common = Adam(lr=1E-3, beta_1=0.9, beta_2=0.999, epsilon=1e-08)

        # create Generator and Discriminator
        self.m_genAB.compile(loss='mae', optimizer=opt_common)
        self.m_genBA.compile(loss='mae', optimizer=opt_common)
        self.m_discriA.trainable = True
        self.m_discriA.compile(loss='binary_crossentropy', optimizer=opt_common)
        self.m_discriB.trainable = True
        self.m_discriB.compile(loss='binary_crossentropy', optimizer=opt_common)

        # build DCGAN
        loss = [l1_loss, 'binary_crossentropy', l1_loss]
        loss_w = [1E1, 1, 1E1]
        self.m_discriB.trainable = False
        self.m_cyc_AB = self.create_cyclegan_AB()
        self.m_cyc_AB.compile(loss=loss, loss_weights=loss_w, optimizer=opt_common)
        self.m_discriA.trainable = False
        self.m_cyc_BA = self.create_cyclegan_BA()
        self.m_cyc_BA.compile(loss=loss, loss_weights=loss_w, optimizer=opt_common)

    def get_discriminator(self, shape, name):
        min_size = min(shape[:-1])
        depth = int(np.ceil(np.log(min_size)/np.log(2)))
        sp_inp_n = "{}_input".format(name)
        sp_inp = Input( shape, name=sp_inp_n )
        judge = discriminator(sp_inp, depth, name)
        discri = Model( inputs=[sp_inp], outputs=[judge], name=name)
        return discri

    def create_cyclegan_AB(self, model_name="cycleABA"):
        inpA = Input(shape=self.sh_A, name='GAN_A_input')
        fakeB = self.m_genAB(inpA)
        judgeAB = self.m_discriB(fakeB)
        idnA = self.m_genBA(fakeB)
        #judgeABA = self.m_discriA(idnA)
        return Model( inputs=[inpA], outputs=[fakeB, judgeAB, idnA], name=model_name )

    def create_cyclegan_BA(self, model_name="cycleBAB"):
        inpB = Input(shape=self.sh_B, name='GAN_B_input')
        fakeA = self.m_genBA(inpB)
        judgeBA = self.m_discriA(fakeA)
        idnB = self.m_genAB(fakeA)
        # judgeBAB = self.discriB(idnB)
        return Model( inputs=[inpB], outputs=[fakeA, judgeBA, idnB], name=model_name )

    def get_generator(self, k):
        inp = Input( shape=k.in_sh, name=k.layername("input") )
        out = unet_with_upsample(inp, k.out_sh, k.depth, k.basename())
        #gen = Model( inputs=[inp], outputs=[out], name=k.layername("generator"))
        gen = Model( inputs=[inp], outputs=[out])
        return gen

    def train(self, X_t_A, X_t_B, X_v_A, X_v_B, training_times):
        img_shape = X_v_A.shape[1:]
        epoc_sz, bat_elem_sz = training_times
        t_num = X_t_A.shape[0]
        v_num = X_v_A.shape[0]
        b_loop_time = t_num // bat_elem_sz


        for e in range(epoc_sz):
            starttime = time.time()
            perm = np.random.permutation(X_t_A.shape[0])
            X_t_A_shuf = X_t_A[perm]
            X_t_B_shuf = X_t_B[perm]
            X_t_A_split = [X_t_A_shuf[i*bat_elem_sz:(i+1)*bat_elem_sz] for i in range(b_loop_time)]
            X_t_B_split = [X_t_B_shuf[i*bat_elem_sz:(i+1)*bat_elem_sz] for i in range(b_loop_time)]

            progbar = k_utl.Progbar(6724)
            for b_idx, (x_A, x_B) in zip(range(b_loop_time), zip(X_t_A_split, X_t_B_split)):
                # Unfreeze the discriminator
                self.m_discriB.trainable = True
                self.m_discriA.trainable = True
                answer = np.zeros((bat_elem_sz, 2), dtype=np.uint8)
                is_fake = b_idx % 2 == 0
                answer[:, is_fake] = 1
                if is_fake:
                    x_susp_B = self.m_genAB.predict(x_A)
                    x_susp_A = self.m_genBA.predict(x_B)
                else:
                    x_susp_B = x_B
                    x_susp_A = x_A
                discri_loss_B = self.m_discriB.train_on_batch(x_susp_B, answer)
                discri_loss_A = self.m_discriA.train_on_batch(x_susp_A, answer)

                # Freeze the discriminator
                challange_idxs = np.random.choice(t_num, bat_elem_sz)
                x_chall_A, x_chall_B = X_t_A[challange_idxs], X_t_B[challange_idxs]
                self.m_discriB.trainable = False
                self.m_discriA.trainable = False
                chall_genu = np.zeros((bat_elem_sz, 2), dtype=np.uint8)
                chall_fake = np.zeros((bat_elem_sz, 2), dtype=np.uint8)
                chall_genu[:, False] = 1
                chall_fake[:, True] = 1
                chall_loss_A = self.m_cyc_AB.train_on_batch(x_chall_A, [x_chall_B, chall_genu, x_chall_A])
                chall_loss_B = self.m_cyc_BA.train_on_batch(x_chall_B, [x_chall_A, chall_genu, x_chall_B])

            progbar.add( b_loop_time
                       , values=[ ("D loglossB", discri_loss_B)
                                , ("D loglossA", discri_loss_A)
                                , ("G tot"     , chall_loss_A[0])
                                , ("G L1 fake" , chall_loss_A[1])
                                , ("G logloss" , chall_loss_A[2])
                                , ("G L1 idnt" , chall_loss_A[3])
                                , ("G tot"     , chall_loss_B[0])
                                , ("G L1 fake" , chall_loss_B[1])
                                , ("G logloss" , chall_loss_B[2])
                                , ("G L1 ident", chall_loss_B[3])
                                ]
                       )
            if b_idx % 30 == 0:
                save_idxs = np.random.choice(X_v_A.shape[0], 24)
                x_sv_A, x_sv_B = X_v_A[save_idxs], X_v_B[save_idxs]
                self.save_image(x_sv_A, x_sv_B, "{}_{}".format(str(e), str(b_idx)))
        print('Epoch %s/%s, Time: %s' % (e + 1, epoc_sz, time.time() - starttime))

    def save_image(self, x_A, x_B, name):
        fakeB = self.m_genAB.predict(x_A)
        fakeA = self.m_genAB.predict(x_B)
        sv = None
        for i, i_A in enumerate(x_A):
            im1 = denormalize_x(i_A)
            im2 = denormalize_x(fakeA[i])
            im3 = denormalize_x(fakeB[i])
            im4 = denormalize_x(x_B[i])
            im12 = cv2.vconcat([im1, im2])
            im123 = cv2.vconcat([im12, im3])
            im1234 = cv2.vconcat([im123, im4])
            if sv is None:
                sv = im123
            else:
                sv = cv2.hconcat([sv, im1234])
        sv = sv * 255
        sv = sv.astype(np.uint8)
        cv2.imwrite(os.path.join(PIC_DIR_PATH, "{}.png".format(name)), sv)





def l1_loss(y_true, y_pred):
    return K.sum(K.abs(y_pred - y_true), axis=-1)


def unet_with_upsample(unet_input, out_shape, depth, name=""):
    x = unet_input
    upcv_shift_infos = []

    # prepare for conv
    def get_cv_name(i):
        return "{}_unet_conv2D_{}".format(name, i+1)
    cv_win = (3,3)
    cv_srd = (2,2)
    cv_pad = "same"
    cv_chan = [ min(512, 64*(2**x)) for x in range(depth)] # like [64, 126, 256, ...]
    # convolution2d
    for n in range(depth):
        cv_lay = Conv2D( cv_chan[n], cv_win, strides=cv_srd, padding=cv_pad, name=get_cv_name(n) )
        if n == 0:
            x = cv_lay(x)
        else:
            x = LeakyReLU( 0.2 )(x)
            x = cv_lay(x)
            x = BatchNormalization( axis=-1 )(x)
        if n < depth - 1:
            upcv_shift_infos.append(x)

    # prepare for upsmpl
    def get_upsm_name(i):
        return "{}_unet_up_Upsmpl_{}".format(name, i+1)
    def get_upcv_name(i):
        return "{}_unet_up_Conv2D_{}".format(name, i+1)
    upcv_win = cv_win
    upcv_srd = cv_srd
    upcv_pad = cv_pad
    upcv_chan = cv_chan[::-1][1:] # like [..., 256, 128, 64]
    # upsampling2d
    for n in range(depth - 1):
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
    x = Conv2D( out_shape[-1], upcv_win, name="{}_last_conv".format(name), padding=upcv_pad )(x)
    x = Activation( 'tanh' )(x)

    return x


def discriminator(sp_input, depth, name=''):
    # input
    x = sp_input

    # prepare for conv
    def get_cv_name(i):
        return "{}_conv2D_{}".format(name, i+1)
    cv_win = (3,3)
    cv_srd = (2,2)
    cv_pad = "same"
    cv_chan = [ min(512, 64*(2**x)) for x in range(depth)] # like [64, 126, 256, ...]
    i = 0
    for n in range(0, depth):
        x = Conv2D( cv_chan[n], cv_win, strides=cv_srd, name=get_cv_name(n), padding=cv_pad)(x)
        x = BatchNormalization( axis=-1 )(x)
        x = LeakyReLU( 0.2 )(x)
    # flatten to (N) and dence
    x = Flatten()(x)
    x = Dense( 2, activation='softmax', name='{}_Dense'.format(name) )(x)

    return x



if __name__=="__main__":
    main()


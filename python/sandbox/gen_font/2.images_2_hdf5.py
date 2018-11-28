# common modules
import os
# piped modules
import cv2
import numpy as np
import h5py
from imutils import paths
from sklearn.preprocessing   import LabelBinarizer
from sklearn.model_selection import train_test_split


DATA_DIR = "data"
TRAIN_DIR = "train"
TRAIN_DIR_PATH = os.path.join(DATA_DIR, TRAIN_DIR)
TARGET_DIR = "target"
TARGET_DIR_PATH = os.path.join(DATA_DIR, TARGET_DIR)
OUTPUT_DIR = "hdf5"
OUTPUT_DIR_PATH = os.path.join(DATA_DIR, OUTPUT_DIR)


def main():
    f_names, labels = get_labels()
    original_imgs = load_data(f_names, TRAIN_DIR_PATH)
    genuin_imgs = load_data(f_names, TARGET_DIR_PATH)
    if len(original_imgs) == len(genuin_imgs):
        img_pairs = list(zip(original_imgs, genuin_imgs))
        X_t_og, X_t_gn, _, X_v_og, X_v_gn, _, lb_hot = refact_dataset_for_keras(img_pairs, labels)

        h5_path = os.path.join(OUTPUT_DIR_PATH, "dataset.hdf5")
        with h5py.File(h5_path, 'w') as f:
            f.create_dataset('train_data_original', data=X_t_og)
            f.create_dataset('train_data_genuin'  , data=X_t_gn)
            f.create_dataset('valid_data_original', data=X_v_og)
            f.create_dataset('valid_data_genuin'  , data=X_v_gn)
    else:
        print("2 images is NOT same number.")


def get_labels():
    f_names = []
    labels = []
    for img_path in paths.list_images(TRAIN_DIR_PATH):
        fn = img_path.split(os.path.sep)[-1]
        f_names.append(fn)
        labels.append(fn.split("_")[0])
    return f_names, labels


def load_data(f_names, dir_path):
    i = 0
    data = []
    for fn in f_names:
        img = cv2.imread(os.path.join(dir_path, fn))
        img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        # Add extra dimension into the image to make Keras happy
        data.append(np.expand_dims(img_gray, axis=2))
    data = np.array(data, dtype="float") / 255.0
    return data


def refact_dataset_for_keras(data, labels):
    # Split the training data into separate train and valid
    X_t, X_v, Y_t_raw, Y_v_raw = train_test_split( data , labels , test_size=0.2 , random_state=0)
    # Convert the labels (letters) into one-hot encodings that Keras can work with
    X_t_og, X_t_gn = unzip(X_t)
    X_v_og, X_v_gn = unzip(X_v)
    lb_hot = LabelBinarizer().fit(labels)
    Y_t = lb_hot.transform(Y_t_raw)
    Y_v = lb_hot.transform(Y_v_raw)
    return np.array(X_t_og), np.array(X_t_gn), np.array(Y_t), np.array(X_v_og), np.array(X_v_gn), np.array(Y_v), lb_hot


def unzip(li):
    return map(list, zip(*li))


if __name__ == "__main__":
    main()


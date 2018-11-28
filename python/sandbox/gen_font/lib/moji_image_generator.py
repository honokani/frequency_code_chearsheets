# common modules
import os
import random
# piped modules
import cv2
import numpy as np
from PIL import ImageFont, Image
# my modules
import lib.utilities as utl

def main():
    data_dir = "data"
    output_dir = "train"
    mojis = ["あ", "い"]
    fontpath ='C:\Windows\Fonts\meiryo.TTC'
    font = ImageFont.truetype(fontpath, 12)
    for _ in range(10):
        make_moji_images(os.path.join(data_dir, output_dir), font, mojis)


def make_moji_images(o_dir, font, ss):
    base_pos = (4, 2)
    noize_size = 4
    gp = gen_pos_noized(base_pos, noize_size)
    for s in ss:
        pos = gp.__next__()
        s_dir = os.path.join(o_dir, s)
        pilimg = make_train_image(s, font, pos)
        utl.save_img(pilimg, s_dir, "{}_{}.png".format(*list(map(str,pos))))


def gen_pos_noized(b_pos, n_size):
    while 1:
        x_noize = random.random() * (n_size * 2) - n_size
        y_noize = random.random() * (n_size * 2) - n_size
        yield int(b_pos[0] + x_noize), int(b_pos[1] + y_noize)


def make_train_image(s, font, pos, size=(20,20,3)):
    white_img = np.zeros(size, dtype=np.uint8)
    white_img.fill(255)
    return utl.put_str_on_img_with_font(white_img, s, font, pos)


if __name__ == "__main__":
    main()


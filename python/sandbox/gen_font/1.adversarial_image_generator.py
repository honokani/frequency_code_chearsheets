# common modules
import os
from enum import Enum
# piped modules
from PIL import ImageFont, Image
# my modules
import lib.moji_image_generator as mi_gen
import lib.mojis_supprier as m_sup
import lib.utilities as utl

ORIGIN_DIR = "original_moji"
DATA_DIR = "data"
TRAIN_DIR = "train"
TARGET_DIR = "target"
IMAGE_LIST = "img_list.txt"

ORG_FONTPATH ='C:\Windows\Fonts\msmincho.TTC'
TGT_FONTPATH ='C:\Windows\Fonts\KouzanMouhituFont.ttf'
IMG_SIZE = (16, 16, 3)


class Font(Enum):
    ORG = (os.path.join(DATA_DIR, TRAIN_DIR), ORG_FONTPATH, 14)
    TGT = (os.path.join(DATA_DIR, TARGET_DIR), TGT_FONTPATH, 16)
    def __init__(self, save_dir, ttf, size):
        self.save_dir = save_dir
        self.ttf = ttf
        self.font_size = size
    def get_font(self):
        return ImageFont.truetype(self.ttf, self.font_size)


def main():
    adv_fonts = [Font.ORG, Font.TGT]
    all_mojis = m_sup.get_train_targets(os.path.join(DATA_DIR, ORIGIN_DIR))

    for _ in range(6):
        for moji in all_mojis:
            make_train_images(adv_fonts, moji)


def make_train_images(fonts, ss):
    base_pos = (1, 1)
    noize_size = 3
    gp = mi_gen.gen_pos_noized(base_pos, noize_size)

    for s in ss:
        pos = gp.__next__()
        list(map(lambda x: make_train_image(s, x, pos), fonts))

def make_train_image(s, font, pos):
    pimg = mi_gen.make_train_image(s, font.get_font(), pos, IMG_SIZE)
    fname = "{}_{}_{}.png".format(str(ord(s)), *list(map(str,pos)))
    utl.save_img(pimg, font.save_dir, fname)


if __name__ == "__main__":
    main()


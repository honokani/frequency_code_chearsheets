import os
import re
import cv2
from PIL import ImageFont, ImageDraw, Image

def get_contets_names(fPath):
    files = []
    dirs = []
    for tgt in os.listdir(fPath):
        if (os.path.isfile(os.path.join(fPath, tgt))):
            files.append(tgt)
        else:
            dirs.append(tgt)
    return dirs, files


def find_dir_by_fullname(d_path, d_name):
    if not d_name == "":
        dirs, _ = get_contets_names(d_path)
        tgt = re.compile("^{}$".format(d_name.lower()))
        for d in dirs:
            if tgt.search(d.lower()) is not None:
                return True, d
    return False, None


def find_dirs_by_name(d_path, d_name=""):
    dirs, _ = get_contets_names(d_path)
    if (d_name == ""):
        return dirs
    else:
        founds = []
        d_name = d_name.lower()
        tgt = re.compile("{}".format(d_name))
        for d in dirs:
            if tgt.search(d.lower()) is not None:
                founds.append(d)
        return founds


def find_file_by_fullname(d_path, f_name):
    if not d_name == "":
        _, files = get_contets_names(d_path)
        tgt = re.compile("^{}$".format(f_name.lower()))
        for f in files:
            if tgt.search(f.lower()) is not None:
                return True, f
    return False, None


def find_files_by_name(d_path, f_name=""):
    _, files = get_contets_names(d_path)
    if (f_name == ""):
        return files
    else:
        founds = []
        f_name = f_name.lower()
        tgt = re.compile("{}".format(f_name))
        for f in files:
            if tgt.search(f.lower()) is not None:
                founds.append(f)
        return founds


def put_str_on_img_with_font(img, s, font, pos):
    pil_img = Image.fromarray(img)
    draw = ImageDraw.Draw(pil_img)
    draw.text(pos, s, font=font, fill=(0,0,0,0))
    return pil_img

def save_img(pilimg, d_path, name):
    if not os.path.isdir(d_path):
        os.makedirs(d_path)
    pilimg.save(os.path.join(d_path, name))


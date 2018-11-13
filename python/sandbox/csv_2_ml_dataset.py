import csv
import numpy as np


def csv_2_list(f_path):
    ls = []
    with open(f_path, 'r') as f:
        reader = csv.reader(f)
        ls = list(reader)
    return ls


def csv_2_list_with_header(f_path):
    ls = csv_2_list(f_path)
    return ls[0], ls[0:]


def csv_2_np_array(f_path):
    return np.array(csv_2_list(f_path))


def csv_2_np_array_with_header(f_path):
    arr = csv_2_np_array(f_path)
    return arr[0], arr[0:]


def split_datas(datas, ratio=0.8):
    def get_shuffle(ls):
        np.random.shuffle(ls)
        return ls
    d_len = len(datas)
    t_len = int(d_len * ratio)
    shuffled_idx = get_shuffle(list(range(d_len)))
    return datas[shuffled_idx[:t_len]], datas[shuffled_idx[t_len:]]


def split2D_xs_y(arr, y_pos=-1):
    return arr[:,:y_pos], arr[:,y_pos]


def make_dic_by_raw_y(ystrss):
    dic = {}
    count = 0
    for ystrs in ystrss:
        for ystr in ystrs:
            if not ystr in dic:
                dic[ystr] = count
                count = count + 1
    return dic


def raw_y_2_int(dic, ystrs):
    return np.array(list(map(lambda y: dic[y], ystrs)))


def raw_ys_2_int(ystrss):
    dic = make_dic_by_raw_y(ystrss)
    return dic, map(lambda x:raw_y_2_int(dic, x), ystrss)


def csv_2_ml_dataset_core(f, f_path, ratio):
    head, tail = f(f_path)
    train, correct = split_datas(tail, ratio)

    train_x, train_y_raw = split2D_xs_y(train)
    correct_x, correct_y_raw = split2D_xs_y(correct)

    raw_ys = [train_y_raw, correct_y_raw]
    dic, [train_y, correct_y] = raw_ys_2_int(raw_ys)
    return train_x, train_y, correct_x, correct_y, dic, head


def csv_2_ml_dataset(f_path, ratio=0.8):
    return csv_2_ml_dataset_core(csv_2_np_array, f_path, ratio)


def csv_with_header_2_ml_dataset(f_path, ratio=0.8):
    return csv_2_ml_dataset_core(csv_2_np_array_with_header, f_path, ratio)


def main():
    f_path = "./data/iris_jp.csv"
    t_c_ratio = 0.8
    test_x, test_y, corr_x, corr_y, dic, head = csv_with_header_2_ml_dataset(f_path, t_c_ratio)


if __name__ == "__main__":
    main()


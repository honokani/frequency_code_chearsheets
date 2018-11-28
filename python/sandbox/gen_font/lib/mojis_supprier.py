# common modules
import os
# my modules
import lib.utilities as utl


def main():
    datas = "data"
    origin_dir = "original_moji"
    train_dir = os.path.join(datas, train_dir)

    ms = get_train_targets(train_dir)
    print(ms)


def get_train_targets(o_dir):
    def split_csv(s):
        return s.split(",")

    contents = []
    for f_name in utl.find_files_by_name(o_dir, "txt"):
        with open(os.path.join(o_dir, f_name), "r") as f:
            c = f.read()
            contents.extend(split_csv(c))

    return contents

if __name__ == "__main__":
    main()


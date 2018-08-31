import time      as TM
import threading as TH

event = TH.Event()
IMAGE = 0

# mock of segmentation process
def create_image():
    global IMAGE
    while(True):
        while not event.wait(1):
            TM.sleep(5)
            IMAGE = IMAGE+1
            print("updated")
        event.clear()

def get_image():
    global IMAGE
    print("check state...")
    event.set()
    return IMAGE


def test_mt(cs):
    # create another thread
    thread = TH.Thread(target=create_image)
    thread.start()

    frame = 0
    old_frame = 0
    while(True):
        TM.sleep(cs)

        old_frame = frame
        frame = get_image()
        if(old_frame != frame):
            print("changed!",frame)
        else:
            print("stable.",frame)

#############################################

def main():
    check_span = 1 # 1 sec
    test_mt(check_span)

if(__name__ == "__main__"):
    main()


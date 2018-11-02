from threading import Event, Lock


class MVar():
    def __init__(self, v=None):
        self._lock = Lock()
        self._val = v
        self._e_i = Event()
        self._e_o = Event()
        if v is None:
            self._e_i.set()
        else:
            self._e_o.set()

    # core methods
    def _put_v_core(self, v):
        b = False
        with self._lock:
            curr_v = self._val
            b = curr_v is None
            if b:
                self._val = v
        return b, b

    def _read_v_core(self):
        curr_v = self._val
        return curr_v is not None, curr_v

    def _take_v_core(self):
        return self._swap_v_core(None)

    def _swap_v_core(self, v):
        b = False
        curr_v = None
        with self._lock:
            curr_v = self._val
            b = curr_v is not None
            if b:
                self._val = v
        return b, curr_v

    def _sychronize_action_io(self, e_itself, e_another, timeout, f, *x):
        taken = None
        in_time = True
        while(in_time):
            in_time = e_itself.wait(timeout)
            e_itself.clear()
            b, taken = f(*x)
            if b:
                break
        e_another.set()
        return taken

    def _sychronize_action_i(self, timeout, f, *x):
        return self._sychronize_action_io(self._e_i, self._e_o, timeout, f, *x)

    def _sychronize_action_o(self, timeout, f, *x):
        return self._sychronize_action_io(self._e_o, self._e_i, timeout, f, *x)

    # common methods
    # 同期書き込み_タイムアウト有り
    def put_with_timeout(self, timeout, v):
        return self._sychronize_action_i(timeout, self._put_v_core, v)

    # 同期読み取り_タイムアウト有り
    def read_with_timeout(self, timeout):
        return self._sychronize_action_o(timeout, self._read_v_core)

    # 同期取り出し_タイムアウト有り
    def take_with_timeout(self, timeout):
        return self._sychronize_action_o(timeout, self._take_v_core)

    # 同期交換_タイムアウト有り
    def swap_with_timeout(self, timeout, v):
        return self._sychronize_action_o(timeout, self._swap_v_core, v)

    # 同期書き込み
    def put(self, v):
        return self.put_with_timeout(None, v)

    # 同期読み取り
    def read(self):
        return self.read_with_timeout(None)

    # 同期取り出し
    def take(self):
        return self.take_with_timeout(None)

    # 同期交換
    def swap(self, v):
        return self.swap_with_timeout(None, v)

    # 非同期書き込み
    def try_put(self, v):
        b, _ = self._put_v_core(v)
        if b:
            self._e_i.clear()
            self._e_o.set()
        return b

    # 非同期取り出し
    def try_take(self):
        b, taken = self._take_v_core()
        if b:
            self._e_o.clear()
            self._e_i.set()
        return b, taken

    # _valに値が入っていないかをチェック
    def is_empty(self):
        return self._val is None

    # _valに値が入っているかをチェック
    def isfilled(self):
        return self._val is not None


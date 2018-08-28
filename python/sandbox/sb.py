def concat_test():
    print("concat_test")
    import numpy as np

    (h,w) = (3,4)
    arr_a = np.arange(h*w)
    arr_a = arr_a.reshape( (h,w,1) )
    print("a_shape:{}".format(arr_a.shape))

    v_b = [ [[6]  ,[7]  ,[8]  ,[9]  ]
          , [[60] ,[70] ,[80] ,[90] ]
          , [[600],[700],[800],[900]]
          ]
    arr_b = np.array( v_b )
    print("b_shape:{}".format(arr_b.shape))

    print( np.concatenate([arr_a,arr_b], axis=-1) )
    print( np.concatenate([arr_a,arr_b], axis=0) )
    print( np.concatenate([arr_a,arr_b], axis=1) )
    print( np.concatenate([arr_a,arr_b], axis=2) )


def do_test():
    concat_test()

if (__name__=="__main__"):
    print("hi")
    do_test()
    print("bye")


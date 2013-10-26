(ns novak-tetris.const)

(def piecedefs
  [{:color [0 255 255]
    :rots [{:shape [[1]
                    [1]
                    [1]
                    [1]]
            :center [0 1]}
           {:shape [[1 1 1 1]]
            :center [2 0]}]}

   {:color [0 0 255]
    :rots [{:shape [[1 1]
                    [1 0]
                    [1 0]]
            :center [0 1]}
           {:shape [[1 1 1]
                    [0 0 1]]
            :center [1 0]}
           {:shape [[0 1]
                    [0 1]
                    [1 1]]
            :center [1 1]}
           {:shape [[1 0 0]
                    [1 1 1]]
            :center [1 1]}]}

   {:color [255 127 0]
    :rots [{:shape [[1 0]
                    [1 0]
                    [1 1]]
            :center [0 1]}
           {:shape [[1 1 1]
                    [1 0 0]]
            :center [1 0]}
           {:shape [[1 1]
                    [0 1]
                    [0 1]]
            :center [1 1]}
           {:shape [[0 0 1]
                    [1 1 1]]
            :center [1 1]}]}

   {:color [255 255 0]
    :rots [{:shape [[1 1]
                    [1 1]]
            :center [0 0]}
           {:shape [[1 1]
                    [1 1]]
            :center [0 0]}]}

   {:color [0 255 0]
    :rots [{:shape [[1 0]
                    [1 1]
                    [0 1]]
            :center [0 1]}
           {:shape [[0 1 1]
                    [1 1 0]]
            :center [1 0]}]}

   {:color [127 0 127]
    :rots [{:shape [[1 0]
                    [1 1]
                    [1 0]]
            :center [0 1]}
           {:shape [[1 1 1]
                    [0 1 0]]
            :center [1 0]}
           {:shape [[0 1]
                    [1 1]
                    [0 1]]
            :center [1 1]}
           {:shape [[0 1 0]
                    [1 1 1]]
            :center [1 1]}]}

   {:color [255 0 0]
    :rots [{:shape [[0 1]
                    [1 1]
                    [1 0]]
            :center [0 1]}
           {:shape [[1 1 0]
                    [0 1 1]]
            :center [0 1]}]}])

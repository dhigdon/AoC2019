;;; Day 3 - wire intersection on a grid
;;; To solve this one, I'm going to write an
;;; interpreter that generates a list of all locations
;;; that the wire is in.
;;; The other option is the model the grid directly.
;;; Modeling the grid directly does make collision easier,
;;; but the grid would have to be arbirartily large in any direction,
;;; so this "sparse" approach is, I think, a better option.
;;;
;;; The wire's locations will be represented as a list of (X.Y)
;;; coordinate pairs.
;;;

;; Input list is comma separated
(require 'cl-ppcre)

(defstruct direction
  (ordinal #\. :type standard-char)
  (distance 0  :type fixnum))

(defun parse-direction (c)
  "c is a direction and distance encoded as [ULDR][0-9]+.
  Return value is (dir-char . distance-fixnum)"
  (make-direction :ordinal (schar c 0)
                  :distance (parse-integer (subseq c 1))))

(defun parse-wire (s)
  "Convert the problem's wire descriptor into our internal format"
  (mapcar #'parse-direction (cl-ppcre:split "," s)))

;;; Coordinates are a pair of fixnums
(defstruct coord
  (row 0 :type fixnum)
  (col 0 :type fixnum))

(defparameter *origin* (make-coord :row 0 :col 0))

;;; Used in our intersection test below.
;;; This test is roughly twice as fast as equalp
(defun coord-eq (a b)
  (declare (type coord a b))
  (and (= (coord-row a) (coord-row b))
       (= (coord-col a) (coord-col b))))

(declaim (ftype (function (coord standard-char) coord)))
(defun coord-move (c dir dist)
  "Creates a new 'coord' one step in 'dir' from the parameter."
  (let ((row (coord-row c))
        (col (coord-col c)))
    (case dir
      (#\U (make-coord :row (+ row dist) :col col))
      (#\D (make-coord :row (- row dist) :col col))
      (#\L (make-coord :row row :col (+ col dist)))
      (#\R (make-coord :row row :col (- col dist))))))

(defun stretch-coords (coords d)
  "Returns a list of coordinates that stretch 'dir' from the given origin.
  The first element in the list is the destination, and the 'coord' list
  is the tail of the list."
  (let ((dir (direction-ordinal d))
        (dst (direction-distance d)))
    (if (<= dst 0)
      coords
      (do* ((i 0 (1+ i))
            (c (car coords) (coord-move c dir 1))
            (r coords (cons c r)))
        ((>= i dst) r)))))

(defun run-wire (coords wire)
  "Generate a list of all coordinates this wire reaches.
  'coords' is a list of where the wire has been, and its head is
  the working end of the wire.
  'wire' is a list of wire commands"
  (if (null wire)
    coords
    (run-wire (stretch-coords coords (car wire))
              (cdr wire))))

(defun manhattan-distance (coord)
  "Note that distance is from 0,0, which is the absolute value"
  (+ (abs (coord-row coord))
     (abs (coord-col coord))))

;;; List intersection
(defun overlaps (path-1 path-2)
  (intersection path-1 path-2 :test #'coord-eq))

;;; Problem solution

(defun nearest-overlap (wire-1 wire-2)
  (let* ((path-1 (run-wire (list *origin*) wire-1))
         (path-2 (run-wire (list *origin*) wire-2)))
    (format t "Starting search - initial path ~A long~%" (length path-1))
    (reduce #'min path-2
            :key #'(lambda (x)
                     (let ((d (manhattan-distance x)))
                       (if (and (not (zerop d))
                                (member x path-1 :test #'coord-eq))
                         d
                         most-positive-fixnum))))))

;;;; Test data
(defvar wire-1 (parse-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72"))
(defvar wire-2 (parse-wire "U62,R66,U55,R34,D71,R55,D58,R83"))

(defvar wire-3 (parse-wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"))
(defvar wire-4 (parse-wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

(defun wire-bounds (wire-path)
  (unless (null wire-path)
    (do* ((path wire-path  (cdr path))
          (c    (car path) (car path))
          (row (coord-row c) (coord-row c))
          (col (coord-col c) (coord-col c))
          (minrow row (min minrow row))
          (maxrow row (max maxrow row))
          (mincol col (min mincol col))
          (maxcol col (max maxcol col)))
      ((null (cdr path)) (list minrow mincol maxrow maxcol))
      (declare (coord c) (fixnum minrow maxrow mincol maxcol)))))

(defun wire-end (wire)
  "Test function - returns the final coordinate of a wire"
  (do ((d (car wire) (car w))
       (w (cdr wire) (cdr w))
       (c *origin*   (coord-move c (direction-ordinal d) (direction-distance d))))
    ((null d) c)))


;;; PuzzleInput

(defvar w1 (parse-wire "R990,D362,L316,U101,R352,U798,L314,D564,R961,D634,L203,U601,R973,U227,R996,D639,L868,D135,L977,D201,R911,D486,R906,U719,L546,U324,R302,D200,L879,D206,L872,U681,R628,D272,R511,D827,L929,U915,L399,U696,R412,D640,R234,U487,R789,U354,L620,D914,L7,D228,L55,D591,L250,D228,R816,U935,R553,U98,L833,D418,R582,D793,R804,U283,R859,D206,L842,U663,L935,U495,L995,D181,R75,D33,R126,U489,L894,D675,R33,U239,L623,D931,L830,U63,R77,D576,L85,D415,R443,U603,R654,U495,L273,U583,R10,D648,L840,U904,R489,D655,R997,U559,L614,U917,R809,U540,L41,U519,R256,U111,R29,D603,L931,U518,R443,D51,L788,U483,L665,U890,L392,D701,R907,D125,L438,D107,L266,U766,R743,D343,R898,U293,L318,U417,L23,U44,L668,U614,R83,U31,R452,U823,R16,D418,R68,U823,L53,D638,L394,D714,R992,U196,R913,D526,L458,U428,L412,U901,R610,U348,L904,D815,R274,U439,R207,D81,L20,D507,L179,U249,L221,U603,L897,U490,R127,U99,L709,U925,L818,D777,R292,U935,R801,U331,R412,U759,L698,D53,L969,U492,L502,D137,R513,D999,L808,D618,L240,U378,L284,D726,L609,U530,R537,D36,L504,D26,R244,D692,L186,U767,L690,U182,R559,D926,R706,D132,L325,D846,R494,U238,L519,U655,R57,U658,L471,D717,L964,D346,L448,U286,L457,D504,R614,U652,R583,D780,R882,U417,R573,D297,L144,U347,L254,D589,L387,U309,L88,D510,R435,U636,L640,U801,R774,U678,R247,D846,L775,U527,L225,U798,R577,U897,R11,U153,L297,D748,L284,U806,R512,U906,L181,U39,R264,D47,L561,D441,L181,U210,L278,U998,R256,D278,R350,U466,L335,D310,L4,U298,L531,D423,R851,U285,L235,D139,R209,U882,R801,D36,L777,D153,L63"))

(defvar w2 (parse-wire "L995,D598,R577,U346,L797,D375,R621,D709,R781,U55,R965,U327,L479,U148,L334,U93,R644,U632,L557,D136,L690,D548,R982,D703,L971,U399,R600,D785,L504,U984,R18,U190,L755,D737,L787,D921,R303,U513,L544,U954,L814,U239,R550,D458,R518,D538,R362,D350,L103,U17,L362,D480,L80,U639,L361,D75,L356,D849,R635,U633,R934,U351,L314,U960,R657,U802,L687,U385,L558,D984,L996,U765,L147,D366,R908,U981,R44,U336,R396,U85,R819,D582,L21,D920,L627,D103,R922,U195,L412,D385,L159,U446,L152,U400,L303,U549,R734,D709,R661,U430,R177,U857,L53,U555,R35,D919,L163,D630,L162,U259,R46,D89,R965,D410,R37,U39,R621,D606,L816,D659,L668,D418,L775,D911,R296,U488,L129,U869,L455,U663,L942,U813,L274,D677,R161,D338,R455,D580,R976,D984,L336,U742,R334,U130,L210,U523,R958,U177,R126,U469,L513,D14,L772,D423,L369,D661,R167,D449,L685,U871,L930,U630,L54,D581,L921,U839,R782,D844,L581,D995,R110,U365,L594,D595,R391,D298,R297,U469,L148,D34,R5,D609,L654,U172,R940,D858,L682,D92,R395,D683,R947,U311,L850,U151,R452,U641,L599,D640,R86,U584,L518,D597,L724,D282,L691,D957,L119,U30,L8,D514,R237,U599,R775,U413,R802,D132,R925,U133,L980,D981,R272,U632,R995,U427,R770,D722,L817,D609,R590,D799,L699,U923,L881,U893,R79,U327,L405,D669,L702,D612,R895,D132,R420,U958,L955,U993,L817,D492,R453,D342,L575,D253,R97,U54,R456,U748,L912,U661,L987,D182,L816,U218,R933,D797,L207,D71,R546,U578,L894,D536,L253,D525,L164,D673,R784,U915,L774,U586,R733,D80,L510,U449,L403,D915,L612,D325,L470,U179,L460,U405,R297,D803,R637,U893,R565,U952,R550,U936,R378,D932,L669"))

;;(format t "Puzzle result = ~A" (nearest-overlap w1 w2))

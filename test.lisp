(defpackage :bukvonesis-test
  (:use :common-lisp :it.bese.FiveAM :bukvonesis))

(in-package :bukvonesis-test)

(def-suite bukvonesis)

(in-suite bukvonesis)

(test distance-squared
  (is (= 0 (bukvonesis::distance-squared 2 2 2 2))))

(test get-best-chromosome-index
  (is (equal (values 0 5) (bukvonesis::get-best-chromosome-index '(5 3 4 2 1))))
  (is (equal (values 1 5) (bukvonesis::get-best-chromosome-index '(1 5 3 4 2))))
  (is (equal (values 4 6) (bukvonesis::get-best-chromosome-index '(1 5 3 4 6)))))

(test coords-distance
  (is (= 0 (bukvonesis::coords-distance '(1 1 2 2 1 1)
					(zpb-ttf::make-control-point 1 1 nil)
					(zpb-ttf::make-control-point 2 2 nil)
					(zpb-ttf::make-control-point 1 1 nil))))

  (is (= 0 (bukvonesis::coords-distance '(1 1 0 0 1 1)
					(zpb-ttf::make-control-point 1 1 nil)
					nil
					(zpb-ttf::make-control-point 1 1 nil)))))
(test evaluate
  (is (= bukvonesis::*max-value* (bukvonesis::evaluate '(1 1 1 1 1 1)
						       (zpb-ttf::make-control-point 1 1 nil)
						       (zpb-ttf::make-control-point 1 1 nil)
						       (zpb-ttf::make-control-point 1 1 nil)))))
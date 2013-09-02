
(in-package :beebster)

(defparameter *test-list-1*
  '("132 b038zh96 http://www.bbc.co.uk/iplayer/images/episode/b038zh96_150_84.jpg Big School Episode 2"
 "133 b039b7jf http://www.bbc.co.uk/iplayer/images/episode/b039b7jf_150_84.jpg Big School Episode 3"
 "134 p01d3nl4 http://www.bbc.co.uk/iplayer/images/episode/p01d3nl4_150_84.jpg Big School Episode 1"
 "299 b01rj3jv http://www.bbc.co.uk/iplayer/images/episode/b01rj3jv_150_84.jpg Extreme School Episode 4"
 "693 b0176qx6 http://www.bbc.co.uk/iplayer/images/episode/b0176qx6_150_84.jpg School for Stars: Series 1 Episode 9"
 "694 b01mxxpw http://www.bbc.co.uk/iplayer/images/episode/b01mxxpw_150_84.jpg School for Stars: Series 2 Episode 6"
 "695 b01myh5v http://www.bbc.co.uk/iplayer/images/episode/b01myh5v_150_84.jpg School for Stars: Series 2 Episode 7"
 "696 b01myhbl http://www.bbc.co.uk/iplayer/images/episode/b01myhbl_150_84.jpg School for Stars: Series 2 Episode 8"
 "697 b01myjlt http://www.bbc.co.uk/iplayer/images/episode/b01myjlt_150_84.jpg School for Stars: Series 2 Episode 9"
 "698 b01mykr0 http://www.bbc.co.uk/iplayer/images/episode/b01mykr0_150_84.jpg School for Stars: Series 2 Episode 10"))

(test test-thumbs-from-string
  (is (equal '("http://www.bbc.co.uk/iplayer/images/episode/b01mykr0_150_84.jpg") (get-thumb-from-search (first (last *test-list-1*))))))

(test test-title-and-episode
 (is (equal '("Big School Episode 2")
	    (get-title-and-episode (first *test-list-1*)))))

(run!)

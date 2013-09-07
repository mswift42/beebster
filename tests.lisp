
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

(defparameter *test-info-1*
  "Matches:
703:	Reading and Leeds Festival - 2013: 15. Don Broco - Reading Festival highlights, BBC Three, Guidance,Music,Rock & Indie,TV, default
INFO: File name prefix = Reading_and_Leeds_Festival_-_2013_15._Don_Broco_-_Reading_Festival_highlights_p01fqbwj_default                 

available:      Unknown
categories:     Music,Rock & Indie
channel:        BBC Three
desc:           Extended highlights of Don Broco's set at Reading Festival 2013
descmedium:     Extended highlights of Don Broco's set at Reading Festival 2013
descshort:      Extended highlights of Don Broco's set at Reading Festival 2013
dir:            /home/martin/quicklisp/local-projects/beebster
dldate:         2013-09-07
dltime:         14:40:45
duration:       2028
durations:      default: 2028
episode:        2013: 15. Don Broco - Reading Festival highlights
episodenum:     15
episodeshort:   2013: Don Broco - Reading Festival highlights
expiry:         2013-09-24T18:01:00Z
expiryrel:      in 17 days 4 hours
ext:            EXT
filename:       /home/martin/quicklisp/local-projects/beebster/Reading_and_Leeds_Festival_-_2013_15._Don_Broco_-_Reading_Festival_highlights_p01fqbwj_default.EXT
filepart:       /home/martin/quicklisp/local-projects/beebster/Reading_and_Leeds_Festival_-_2013_15._Don_Broco_-_Reading_Festival_highlights_p01fqbwj_default.partial.EXT
fileprefix:     Reading_and_Leeds_Festival_-_2013_15._Don_Broco_-_Reading_Festival_highlights_p01fqbwj_default
firstbcast:     default: 2013-08-25T19:01:00+01:00
firstbcastrel:  default: 12 days 19 hours ago
guidance:       adult
index:          703
lastbcast:      default: 2013-08-25T19:01:00+01:00
lastbcastrel:   default: 12 days 19 hours ago
longname:       Reading and Leeds Festival
modes:          default: flashhigh1,flashhigh2,flashlow1,flashlow2,flashstd1,flashstd2,flashvhigh1,flashvhigh2,rtsphigh1,rtsphigh2,rtsplow1,rtsplow2,rtspstd1,rtspstd2,rtspvhigh1,rtspvhigh2
modesizes:      default: flashhigh1=197MB,flashhigh2=197MB,flashlow1=98MB,flashlow2=98MB,flashstd1=119MB,flashstd2=119MB,flashvhigh1=371MB,flashvhigh2=371MB,rtsphigh1=197MB,rtsphigh2=197MB,rtsplow1=98MB,rtsplow2=98MB,rtspstd1=119MB,rtspstd2=119MB,rtspvhigh1=371MB,rtspvhigh2=371MB
name:           Reading and Leeds Festival
nameshort:      Reading and Leeds Festival
pid:            p01fqbwj
player:         http://www.bbc.co.uk/iplayer/episode/p01fqbwj/Reading_and_Leeds_Festival_2013_Don_Broco_Reading_Festival_highlights/
senum:          s00e15
thumbfile:      /home/martin/quicklisp/local-projects/beebster/Reading_and_Leeds_Festival_-_2013_15._Don_Broco_-_Reading_Festival_highlights_p01fqbwj_default.jpg
thumbnail:      http://www.bbc.co.uk/iplayer/images/episode/p01fqbwj_150_84.jpg
thumbnail1:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_86_48.jpg
thumbnail2:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_150_84.jpg
thumbnail3:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_178_100.jpg
thumbnail4:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_512_288.jpg
thumbnail5:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_528_297.jpg
thumbnail6:     http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_640_360.jpg
timeadded:      12 days 16 hours ago (1377464262)
title:          Reading and Leeds Festival: 2013: Don Broco - Reading Festival highlights
type:           tv
verpids:        default: p01fqbwn
version:        default
versions:       default
web:            http://www.bbc.co.uk/programmes/p01fqbwj.html


INFO: 1 Matching Programmes
")

(test test-thumbs-from-string
  (is (equal '("http://www.bbc.co.uk/iplayer/images/episode/b01mykr0_150_84.jpg") (get-thumb-from-search (first (last *test-list-1*))))))

(test test-title-and-episode
 (is (equal '("Big School Episode 2")
	    (get-title-and-episode (first *test-list-1*)))))

(test test-get-info
  (is (equal "get_iplayer -i 703" (get-info 703))))

(test test-get-index-from-search
  (is (equal '("234") (get-index-from-search "234 a_shiny world_2 3 1")))
  (is (equal '("1") (get-index-from-search "1 1_hallo 2 3")))
  (is (equal '("012345") (get-index-from-search "012345 012345 012345"))))

(test test-get-url
  (is (equal "/info?index=666" (get-url "666"))))

(test test-load-thumbnail-for-index
  (is (equal "Reading and Leeds Festival: 2013: Don Broco - Reading Festival highlights"
	     (first (all-matches-as-strings "[A-Z].*"
					    (first (all-matches-as-strings
						    "title:.*"
						    *test-info-1*))))))
  (is (equal "http://ichef.bbci.co.uk/programmeimages/p01fqbvz/p01fqbwj_512_288.jpg" (first (all-matches-as-strings "htt.*"
														    (first (all-matches-as-strings
															    "thumbnail4:.*"
															    *test-info-1*)))))))

(run!)

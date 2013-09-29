beebster
========

Webgui for Get-Iplayer.



To try it out:
-------------

Install [get-iplayer](https://github.com/dinkypumpkin/get_iplayer). 

clone the repository, then start your lisp implementation and 
><pre><code>(load "/full/path/to/beebster.asd")</code></pre>
><pre><code>(ql:quickload "beebster")</code></pre>

change to the beebster package (in Emacs thats **C-c M-p** beebster or <code>(in-package :beebster)</code> from a repl.)


and start a Hunchentoot instance:
><pre><code>(main)</code></pre>

Now open page localhost:4242/search with a webbrowser.


###Download of programmes will only work within the UK.

Screenshots
-----------



![Search](https://github.com/mswift42/beebster/raw/master/Search.png)

![Categories](https://github.com/mswift42/beebster/raw/master/Categorylisting.png)

![Info](https://github.com/mswift42/beebster/raw/master/EpisodeInfo.png)


##TODO

* Add wikipedia-info for programmes.
* show available download-modes.


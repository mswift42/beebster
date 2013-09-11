beebster
========

Webgui for Get-Iplayer.

Still in early stages, however downloading and getting info for programmes works.

To try it out:

Install [get-iplayer](https://github.com/dinkypumpkin/get_iplayer). 

clone the repository, then start your lisp implementation and 
><pre><code>(load "/full/path/to/beebster.asd")</code></pre>
><pre><code>(ql:quickload "beebster")</code></pre>

change to the beebster package (in Emacs thats **C-c M-p** beebster or <code>(in-package :beebster)</code> from a repl.)


and start a Hunchentoot instance:
><pre><code>(start (make-instance 'easy-acceptor :port 4242))</code></pre>

Now open page localhost:4242/search with a webbrowser.



![Search](https://github.com/mswift42/beebster/raw/master/Search.png)

![Categories](https://github.com/mswift42/beebster/raw/master/Categorylisting.png)

![Info](https://github.com/mswift42/beebster/raw/master/EpisodeInfo.png)


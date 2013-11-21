;;;; beebster.asd

(asdf:defsystem #:beebster
  :serial t
  :description "gui for get_iplayer"
  :author "Martin Haesler martin.haesler@gmail.com"
  :license "Copyright:: Copyright (c) 2013, Martin Haesler

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

"
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-ppcre
	       #:inferior-shell
	       #:fiveam)
  :components ((:file "package")
               (:file "beebster")
	       (:file "tests")))


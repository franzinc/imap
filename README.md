imap: A common lisp library for sending and receiving email.
============================================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Examples
 * Open Source 

Description
-----------

A client library for sending and receiving email.

Author
------

John Foderaro, Franz Inc.

Author comments
---------------

The most popular protocol for accessing a mailbox was the Post Office
Protocol (POP) defined in rfc1939.  While it is popular, pop has very
few features. It doesn't allow you to manage the mail on the server
itself, instead you usually just download all mail to your local
machine. A much more powerful protocol called the Internet Message
Access Protocol (IMAP) was defined in the 1996 document rfc2060. With
imap you can work with your mail while it's on the server and can
create folders on the server in which to archive your mail. Thus the
server itself can be the message store which is useful if you want to
access your mail from more than one machine. You are not required to
use the server to archive your mail with imap, you can use it like pop
and download all the mail to your local machine.

Sending email is done via the Simple Mail Transfer Protocol
(SMTP). You can use smtp to send mail directly to the destination but
typically this is not done because the destination machine may be down
or unreachable at the time you wish to send the mail. Most
organizations have a local mail server that is up and reachable all of
the time. You can use smtp to send your letter to that local mail
server and it will then take over the job of getting the mail to the
destination (which may involve queueing the message and retrying to
send it over a period of days).

Platforms
---------

Allegro Common Lisp 7.0 and newer on all platforms.

Dependencies
------------

None, but for the test suite, [tester](http://opensource.franz.com)
is required.

Installation
------------

Start Allegro Common Lisp and load the load.cl file
    :ld /path/to/load.cl

Configuration
-------------

Set the following variables to true for extra debugging information:

    (setq net.post-office::*debug-imap* t 
          net.post-office::*smtp-debug* t)
 
Documentation
-------------

 * [pop and imap interfaces]
   (http://franz.com/support/documentation/current/doc/imap.htm)
 * [smtp interface]
   (http://franz.com/support/documentation/current/doc/imap.htm#smtp-1)
 * Also see the imap.html file that is included with this source code.

License
-------

The aserve source code is licensed under the terms of the 
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these 
conflict, the preamble takes precedence.  This project is referenced in the 
preamble as the LIBRARY.
 
Notes
-----

For reference please see rfc1939 (pop) and rfc2060 (imap).
 
Examples and Information
------------------------

See the first link in the documenation section above for examples.  

Franz Inc. Open Source Info
---------------------------

This project's homepage is <http://opensource.franz.com>. There is an 
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html) 
for these open source projects. We encourage you to take advantage by 
subscribing to the list.  Once you're subscribed, email to 
<opensource@franz.com> with your questions, comments, suggestions, 
and patches.

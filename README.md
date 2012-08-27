**NOTE** this repository is under heavy editing and unstable.

webmachine
==========

Zotonic's Variation
-------------------

This is a fork of Basho's Webmachine.  This fork is done for supporting content management systems with large amounts of dispatch rules and many virtual hosts.


Differences with Basho Webmachine
---------------------------------

The main differences with Basho's Webmachine are:

* Pluggable dispatch handler
* Support for the HTTP Upgrade
* Caching of resource callbacks results
* Dispatch handler can redirect requests
* Use of process dictionary has been removed
* webmachine_request is now a normal (not parametrized) module
* Extra logging

Together this gave a significant speed boost to Webmachine.

In the specific case of Zotonic the difference was 5 milliseconds (or more) per request (on a 2GHz Core 2 Duo). Without these optimizations we were not able to use Webmachine.




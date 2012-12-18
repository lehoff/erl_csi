erl_csi
=================

Simple functions to figure out code structure information about Erlang applications.

 Run the analysis like this:
 1. erl -make above src dir
 2. erl -pz ebin
``` erlang
otp_analysis:start("otp_analysis.cfg").
otp_analysis:core_analysis().
``` 

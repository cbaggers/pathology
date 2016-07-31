This is a little experiment in making a path type that is a simple as possible whilst still being useful and extensible.

The motivation was seeing that incredibly smart folks have tried to wrangle CL's `pathname` type into consistant behaviour and even with all their work it still is lacking [0].

This project very much limits itself to the business of specifying & dealing with paths and not the system level business of validating/using them in any way. If this experiment results in something pleasent then I will look at hooking into some of the other excellent libraries for doing OS level stuff.

[0] See appendix C of Fare's wonderful history of asdf for details http://fare.tunes.org/files/asdf3/asdf3-2014.html#%28part._pathnames%29

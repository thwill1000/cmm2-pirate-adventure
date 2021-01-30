This is a small adventure that I created and implemented in a 24-hour
period in September of 2007.  This is an original adventure for the
Scott Adams' adventure interpreter language, and is, to the extent of
my knowledge, the first original adventure for Scott's language in over
20 years.

While Inform, TADS, and what not are more capable than Scott's language
at making complicated adventures, Scott's system has the advantage of being
extremely compact, being originally designed to run on a system with only
16 or 24 kilobytes (yes, kilobytes) of memory.  For example, my implemention
of Cloak of Darkness for Scott's system is under 3,000 bytes in size.
Compare this to the Z-machine version of the same game, which is over 50,000
bytes in size.

The program to run Scott Adams' adventures is under 20k in size; the 
Z machine interpreter is about 80k in size, and other interpreters
are even bigger.

My goal in making this adventure was to make a playable, very small 
adventure that people can freely include with programs like ScottFree.

In particular, I wanted the adventure to fit in under 6 2048-byte blocks,
the size of a sector when making an ISO9660 filesystem.

To play this game on Windows, use the included ADVENT.EXE program, which is
a port of ScottFree to Windows.  People using other systems are free to 
compile the included ScottFree program, which I have updated to run
without problem on modern versions of GCC.  

People on non-Windows systems who do not feel comfortable compiling
programs, but who have a Z-machine interpreter, can try the desert.z5
file.  Note that desert.z5 was compiled using an ancient version of Inform
(Inform 5), and has some formatting problems with some Z-machine terps,
such as Gargoyle.  It works perfectly, however, in the non-GUI version
of Frotz.

The source code and scripts I used to "compile" this adventure are in the
soruce/ directory.


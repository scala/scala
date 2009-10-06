

#export PATH=${PATH}:/usr/lib/gcc-lib/i686-pc-cygwin/3.3.3
JDK='/C/Program Files/Java/jdk1.5.0/'

native:
	#gcc -I'C:/Program Files/Java/'*'/include/' -I'C:/Program Files/Java/'*'/include//win32/' -mno-cygwin -Wl,--add-stdcall-alias -shared -o jline.dll jline_WindowsTerminal.c
	gcc -L /usr/lib/mingw/ -I${JDK}/include -I${JDK}/include/win32 -mwindows -mno-cygwin -Wl,--add-stdcall-alias -shared -o jline.dll jline_WindowsTerminal.c

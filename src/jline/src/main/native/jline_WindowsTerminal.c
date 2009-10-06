#include "jline_WindowsTerminal.h"
#include <windows.h>

JNIEXPORT jint JNICALL Java_jline_WindowsTerminal_getConsoleMode
  (JNIEnv *env, jobject ob)
{
	DWORD mode;
	HANDLE hConsole = GetStdHandle (STD_INPUT_HANDLE);

	if (hConsole == INVALID_HANDLE_VALUE)
		return -1;

	if (!GetConsoleMode (hConsole, &mode))
		return -1;

	// CloseHandle (hConsole);

	// printf ("JNI get mode=%d\n", mode);
	return mode;
}

JNIEXPORT void JNICALL Java_jline_WindowsTerminal_setConsoleMode
  (JNIEnv *env, jobject ob, jint mode)
{
	DWORD m = (DWORD)mode;
	HANDLE hConsole = GetStdHandle (STD_INPUT_HANDLE);

	if (hConsole == INVALID_HANDLE_VALUE)
		return;

	// printf ("JNI set mode=%d\n", m);
	SetConsoleMode (hConsole, m);
	// CloseHandle (hConsole);
}

JNIEXPORT jint JNICALL Java_jline_WindowsTerminal_readByte (JNIEnv * env, jclass class)
{
	return getch ();
}

JNIEXPORT jint JNICALL Java_jline_WindowsTerminal_getWindowsTerminalWidth (JNIEnv * env, jclass class)
{
	HANDLE inputHandle = GetStdHandle (STD_INPUT_HANDLE);
	HANDLE outputHandle = GetStdHandle (STD_OUTPUT_HANDLE);
	PCONSOLE_SCREEN_BUFFER_INFO info =  malloc (sizeof (CONSOLE_SCREEN_BUFFER_INFO));
	GetConsoleScreenBufferInfo (outputHandle, info);
	return info->srWindow.Right - info->srWindow.Left+1;
}

JNIEXPORT jint JNICALL Java_jline_WindowsTerminal_getWindowsTerminalHeight (JNIEnv * env, jclass class)
{
	HANDLE inputHandle = GetStdHandle (STD_INPUT_HANDLE);
	HANDLE outputHandle = GetStdHandle (STD_OUTPUT_HANDLE);
	PCONSOLE_SCREEN_BUFFER_INFO info =  malloc (sizeof (CONSOLE_SCREEN_BUFFER_INFO));
	GetConsoleScreenBufferInfo (outputHandle, info);
	return info->srWindow.Bottom - info->srWindow.Top+1;
}

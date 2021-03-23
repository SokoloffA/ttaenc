/*
 * ttaenc.h
 *
 * Description: TTAv1 encoder definitions and prototypes
 * Copyright (c) 2007, Aleksander Djuric (ald@true-audio.com)
 * Distributed under the GNU General Public License (GPL).
 * The complete text of the license can be found in the
 * COPYING file included in the distribution.
 *
 */

#ifndef TTAENC_H
#define TTAENC_H

#ifdef _MSC
#include <windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef __GNUC__
#define __USE_ISOC99
#endif

#include <wchar.h>
#include <wctype.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <locale.h>
#include <fcntl.h>

#ifdef _MSC
#include <direct.h>
#include <io.h>
#include <conio.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#ifdef _MSC
#pragma pack(1)
#define __ATTRIBUTE_PACKED__
#else
#define __ATTRIBUTE_PACKED__	__attribute__((packed))
#endif

#define COPYRIGHT		"Copyright (c) 2007 Aleksander Djuric. All rights reserved."

#define MYNAME			"ttaenc"
#define VERSION			"3.4.1"
#define BUILD			"20070727"
#define PROJECT_URL		"http://tta.sourceforge.net"

#define MAX_BPS			24
#define FRAME_TIME		1.04489795918367346939

#define TTA1_SIGN		0x31415454
#define RIFF_SIGN		0x46464952
#define WAVE_SIGN		0x45564157
#define fmt_SIGN		0x20746D66
#define data_SIGN		0x61746164

#define MAX_ORDER		16
#define BIT_BUFFER_SIZE (1024*1024)

#define WAVE_FORMAT_PCM	1
#define WAVE_FORMAT_EXTENSIBLE 0xFFFE

#define COMMAND_ERROR	0
#define FORMAT_ERROR	1
#define FILE_ERROR	2
#define FIND_ERROR	3
#define CREATE_ERROR	4
#define OPENR_ERROR	5
#define OPENW_ERROR	6
#define MEMORY_ERROR	7
#define WRITE_ERROR	8
#define READ_ERROR	9

#ifndef _MAX_FNAME
#define _MAX_FNAME	1024
#endif

#define LINE "------------------------------------------------------------"

#ifdef _BIG_ENDIAN
#define	ENDSWAP_INT16(x)	(((((x)>>8)&0xFF)|(((x)&0xFF)<<8)))
#define	ENDSWAP_INT32(x)	(((((x)>>24)&0xFF)|(((x)>>8)&0xFF00)|(((x)&0xFF00)<<8)|(((x)&0xFF)<<24)))
#else
#define	ENDSWAP_INT16(x)	(x)
#define	ENDSWAP_INT32(x)	(x)
#endif

#ifdef _WIN32
typedef unsigned __int32 uint32;
typedef unsigned __int64 uint64;
#else
typedef __uint32_t uint32;
typedef __uint64_t uint64;
#endif

#define PREDICTOR1(x, k)	((int)((((uint64)x << k) - x) >> k))
#define ENC(x)  (((x)>0)?((x)<<1)-1:(-(x)<<1))
#define DEC(x)  (((x)&1)?(++(x)>>1):(-(x)>>1))

typedef struct {
	unsigned int k0;
	unsigned int k1;
	unsigned int sum0;
	unsigned int sum1;
} adapt;

typedef struct {
	int shift;
	int round;
	int error;
	int qm[MAX_ORDER];
	int dx[MAX_ORDER];
	int dl[MAX_ORDER];
} fltst;

typedef struct {
	fltst fst;
	adapt rice;
	int last;
} encoder;

#ifdef _MSC
#define strlen wcslen
#define strcasecmp stricmp
#define wfopen(x,y) _wfopen(x,L##y)
#define wunlink _wunlink
#define wstrncpy wcsncpy

#else
#define wstrncpy mbstowcs

FILE* wfopen (wchar_t *wcname, char *mode) {
	char name[_MAX_FNAME * MB_LEN_MAX];
	wcstombs(name, wcname, MB_CUR_MAX * wcslen(wcname) + 1);
	return fopen(name, mode);
}

int wunlink (wchar_t *wcname) {
	char name[_MAX_FNAME * MB_LEN_MAX];
	wcstombs(name, wcname, MB_CUR_MAX * wcslen(wcname) + 1);
	return unlink(name);
}

int wcsicmp (const wchar_t *s1, const wchar_t *s2) {
	wint_t c1, c2;
	if (s1 == s2) return 0;
	do {
		c1 = towlower(*s1++);
		c2 = towlower(*s2++);
		if (c1 == L'\0') break;
	} while (c1 == c2);
	return c1 - c2;
}
#endif

#ifdef _WIN32
	#define _SEP L'\\'
	#define ERASE_STDERR fwprintf (stderr, L"%78c\r", 0x20)
#else
	#define _SEP L'/'
	#define ERASE_STDERR fwprintf (stderr, L"\033[2K")
#endif

#if ((defined(_WIN32)) && (defined(__GNUC__)))
	#define mkdir(file, mode) _mkdir(file)
#endif

#endif	/* TTAENC_H */

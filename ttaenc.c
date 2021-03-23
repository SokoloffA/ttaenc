/*
 * ttaenc.c
 *
 * Description: TTAv1 lossless audio encoder/decoder.
 * Copyright (c) 2007, Aleksander Djuric (ald@true-audio.com)
 * Distributed under the GNU General Public License (GPL).
 * The complete text of the license can be found in the
 * COPYING file included in the distribution.
 *
 */

#include "ttaenc.h"

/******************* static variables and structures *******************/

static unsigned char BIT_BUFFER[BIT_BUFFER_SIZE + 8];
static unsigned char *BIT_BUFFER_END = BIT_BUFFER + BIT_BUFFER_SIZE;
static unsigned char *WAVE_BUFFER;

static struct {
	unsigned int TTAid;
	unsigned short AudioFormat;
	unsigned short NumChannels;
	unsigned short BitsPerSample;
	unsigned int SampleRate;
	unsigned int DataLength;
	unsigned int CRC32;
} __ATTRIBUTE_PACKED__ tta_hdr;

static unsigned int *seek_table;

static struct {
	unsigned char id[3];
	unsigned short version;
	unsigned char flags;
	unsigned char size[4];
} __ATTRIBUTE_PACKED__ id3v2;

static struct {
	unsigned int ChunkID;
	unsigned int ChunkSize;
	unsigned int Format;
	unsigned int Subchunk1ID;
	unsigned int Subchunk1Size;
	unsigned short AudioFormat;
	unsigned short NumChannels;
	unsigned int SampleRate;
	unsigned int ByteRate;
	unsigned short BlockAlign;
	unsigned short BitsPerSample;
} __ATTRIBUTE_PACKED__ wave_hdr;

static struct {
	unsigned int SubchunkID;
	unsigned int SubchunkSize;
} subchunk_hdr;

typedef struct {
	unsigned int f1;
	unsigned short f2;
	unsigned short f3;
	char f4[8];
} EXT_SUBFORMAT;

typedef struct {
	unsigned short cbSize;
	unsigned short validBits;
	unsigned int chMask;
	EXT_SUBFORMAT est;
} EXTENSIBLE_WAV_HDR;

static uint32 frame_crc32;
static uint32 bit_cache;
static uint32 bit_count;

static unsigned char *bitpos;
static unsigned int lastpos;

static struct flist *files_list = NULL;
static struct flist *files_list_tail = NULL;

static FILE *fdin, *fdout;
static wchar_t file_in[_MAX_FNAME];
static wchar_t file_out[_MAX_FNAME];
static wchar_t out_path[_MAX_FNAME];

static unsigned int fixed_out = 0;
static unsigned int clean_src = 0;
static unsigned int wave_ext = 0;

static uint64 total_input_bytes;
static uint64 total_output_bytes;

static unsigned int input_byte_count;
static unsigned int output_byte_count;

const uint32 crc32_table[256] = {
	0x00000000, 0x77073096, 0xee0e612c, 0x990951ba,
	0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
	0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
	0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
	0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
	0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
	0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,
	0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
	0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
	0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
	0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940,
	0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
	0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116,
	0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
	0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
	0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
	0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a,
	0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
	0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818,
	0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
	0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
	0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
	0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c,
	0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
	0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
	0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
	0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
	0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
	0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086,
	0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
	0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4,
	0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
	0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
	0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
	0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
	0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
	0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe,
	0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
	0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
	0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
	0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252,
	0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
	0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60,
	0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
	0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
	0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
	0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04,
	0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
	0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a,
	0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
	0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
	0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
	0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e,
	0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
	0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
	0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
	0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
	0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
	0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0,
	0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
	0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6,
	0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
	0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
	0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

const uint32 bit_mask[] = {
	0x00000000, 0x00000001, 0x00000003, 0x00000007,
	0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
	0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
	0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
	0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
	0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
	0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
	0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
	0xffffffff
};

const uint32 bit_shift[] = {
	0x00000001, 0x00000002, 0x00000004, 0x00000008,
	0x00000010, 0x00000020, 0x00000040, 0x00000080,
	0x00000100, 0x00000200, 0x00000400, 0x00000800,
	0x00001000, 0x00002000, 0x00004000, 0x00008000,
	0x00010000, 0x00020000, 0x00040000, 0x00080000,
	0x00100000, 0x00200000, 0x00400000, 0x00800000,
	0x01000000, 0x02000000, 0x04000000, 0x08000000,
	0x10000000, 0x20000000, 0x40000000, 0x80000000,
	0x80000000, 0x80000000, 0x80000000, 0x80000000,
	0x80000000, 0x80000000, 0x80000000, 0x80000000
};

const uint32 *shift_16 = bit_shift + 4;

struct flist;
struct flist {
	wchar_t fname[_MAX_FNAME];
	struct flist *next;
};

/************************* common functions ****************************/

wchar_t *print_path(wchar_t *filename, int mode)
{
	static wchar_t showname[_MAX_FNAME];
	wchar_t *p;

	if (!mode && (p = wcsrchr(filename, _SEP))) p++;
	else p = filename;
	wcsncpy(showname, p, _MAX_FNAME - 1);

	return showname;
}

void tta_error(int error, wchar_t *name)
{
	ERASE_STDERR;
	switch (error) {
	case COMMAND_ERROR:
	fwprintf(stderr, L"Error:\tunknown command '%ls'\n%hs\n", name, LINE); break;
	case FORMAT_ERROR:
	fwprintf(stderr, L"Error:\tnot compatible file format\n%hs\n", LINE); break;
	case FILE_ERROR:
	fwprintf(stderr, L"Error:\tfile is corrupted\n%hs\n", LINE); break;
	case FIND_ERROR:
	fwprintf(stderr, L"Error:\tfile(s) not found '%ls'\n%hs\n\n", name, LINE); exit(1);
	case CREATE_ERROR:
	fwprintf(stderr, L"Error:\tproblem creating directory '%ls'\n%hs\n\n", name, LINE); exit(1);
	case OPENR_ERROR:
	fwprintf(stderr, L"Error:\tcan't open input file '%ls'\n%hs\n\n", name, LINE); exit(1);
	case OPENW_ERROR:
	fwprintf(stderr, L"Error:\tcan't open output file '%ls'\n%hs\n\n", name, LINE); exit(1);
	case MEMORY_ERROR:
	fwprintf(stderr, L"Error:\tinsufficient memory available\n%hs\n\n", LINE); exit(1);
	case WRITE_ERROR:
	fwprintf(stderr, L"Error:\tcan't write to output file\n%hs\n\n", LINE); exit(1);
	case READ_ERROR:
	fwprintf(stderr, L"Error:\tcan't read from input file\n%hs\n\n", LINE); exit(1);
	}
}

void *tta_malloc(size_t num, size_t size)
{
	void *array;

	if ((array = calloc(num, size)) == NULL)
	tta_error(MEMORY_ERROR, NULL);

	return (array);
}

/************************** crc32 functions ****************************/

#define UPDATE_CRC32(x, crc) crc = \
	(((crc>>8) & 0x00FFFFFF) ^ crc32_table[(crc^x) & 0xFF])

static uint32
crc32 (unsigned char *buffer, unsigned int len) {
	unsigned int i;
	uint32 crc = 0xFFFFFFFF;

	for (i = 0; i < len; i++) UPDATE_CRC32(buffer[i], crc);

	return (crc ^ 0xFFFFFFFF);
}

/************************* bit operations ******************************/

void init_buffer_read(unsigned int pos) {
	frame_crc32 = 0xFFFFFFFFUL;
	bit_count = bit_cache = lastpos = 0;
	bitpos = BIT_BUFFER_END;
	lastpos = pos;
}

void init_buffer_write(unsigned int pos) {
	frame_crc32 = 0xFFFFFFFFUL;
	bit_count = bit_cache = 0;
	bitpos = BIT_BUFFER;
	lastpos = pos;
}

__inline void get_binary(unsigned int *value, unsigned int bits) {
	while (bit_count < bits) {
		if (bitpos == BIT_BUFFER_END) {
			int res = fread(BIT_BUFFER, 1,
					BIT_BUFFER_SIZE, fdin);
			if (!res) {
				tta_error(READ_ERROR, NULL);
				return;
			}
			input_byte_count += res;
			bitpos = BIT_BUFFER;
		}

		UPDATE_CRC32(*bitpos, frame_crc32);
		bit_cache |= *bitpos << bit_count;
		bit_count += 8;
		bitpos++;
	}

	*value = bit_cache & bit_mask[bits];
	bit_cache >>= bits;
	bit_count -= bits;
	bit_cache &= bit_mask[bit_count];
}

__inline void get_unary(unsigned int *value) {
	*value = 0;

	while (!(bit_cache ^ bit_mask[bit_count])) {
		if (bitpos == BIT_BUFFER_END) {
			int res = fread(BIT_BUFFER, 1,
					BIT_BUFFER_SIZE, fdin);
			if (!res) {
				tta_error(READ_ERROR, NULL);
				return;
			}
			input_byte_count += res;
			bitpos = BIT_BUFFER;
		}

		*value += bit_count;
		bit_cache = *bitpos++;
		UPDATE_CRC32(bit_cache, frame_crc32);
		bit_count = 8;
	}

	while (bit_cache & 1) {
		(*value)++;
		bit_cache >>= 1;
		bit_count--;
	}

	bit_cache >>= 1;
	bit_count--;
}

__inline void put_binary(unsigned int value, unsigned int bits) {
	while (bit_count >= 8) {
		if (bitpos == BIT_BUFFER_END) {
			int res = fwrite(BIT_BUFFER, 1,
					BIT_BUFFER_SIZE, fdout);
			if (!res) {
				tta_error(WRITE_ERROR, NULL);
				return;
			}
			output_byte_count += res;
			bitpos = BIT_BUFFER;
		}

		*bitpos = (unsigned char) (bit_cache & 0xFF);
		UPDATE_CRC32(*bitpos, frame_crc32);
		bit_cache >>= 8;
		bit_count -= 8;
		bitpos++;
	}

	bit_cache |= (value & bit_mask[bits]) << bit_count;
	bit_count += bits;
}

__inline void put_unary(unsigned int value) {
	do {
		while (bit_count >= 8) {
			if (bitpos == BIT_BUFFER_END) {
				int res = fwrite(BIT_BUFFER, 1,
						BIT_BUFFER_SIZE, fdout);
				if (!res) {
					tta_error(WRITE_ERROR, NULL);
					return;
				}
				output_byte_count += res;
				bitpos = BIT_BUFFER;
			}

			*bitpos = (unsigned char) (bit_cache & 0xFF);
			UPDATE_CRC32(*bitpos, frame_crc32);
			bit_cache >>= 8;
			bit_count -= 8;
			bitpos++;
		}

		if (value > 23) {
			bit_cache |= bit_mask[23] << bit_count;
			bit_count += 23;
			value -= 23;
		} else {
			bit_cache |= bit_mask[value] << bit_count;
			bit_count += value + 1;
			value = 0;
		}
	} while (value);
}

int done_buffer_write() {
	unsigned int res, bytes_to_write;

	while (bit_count) {
		*bitpos = (unsigned char) (bit_cache & 0xFF);
		UPDATE_CRC32(*bitpos, frame_crc32);
		bit_cache >>= 8;
		bit_count = (bit_count > 8) ? (bit_count - 8) : 0;
		bitpos++;
	}

	frame_crc32 ^= 0xFFFFFFFFUL;
	frame_crc32 = ENDSWAP_INT32(frame_crc32);
	memcpy(bitpos, &frame_crc32, 4);
	bytes_to_write = bitpos + sizeof(int) - BIT_BUFFER;
	res = fwrite(BIT_BUFFER, 1, bytes_to_write, fdout);
	if (!res) {
		tta_error(WRITE_ERROR, NULL);
		return 0;
	}

	output_byte_count += res;
	bitpos = BIT_BUFFER;
	frame_crc32 = 0xFFFFFFFFUL;

	res = output_byte_count - lastpos;
	lastpos = output_byte_count;

	return res;
}

int done_buffer_read() {
	unsigned int crc32, rbytes, res;
	frame_crc32 ^= 0xFFFFFFFFUL;

	rbytes = BIT_BUFFER_END - bitpos;
	if (rbytes < sizeof(int)) {
		memcpy(BIT_BUFFER, bitpos, 4);
		res = fread(BIT_BUFFER + rbytes, 1,
			BIT_BUFFER_SIZE - rbytes, fdin);
		if (!res) {
			tta_error(READ_ERROR, NULL);
			return 1;
		}
		input_byte_count += res;
		bitpos = BIT_BUFFER;
	}

	memcpy(&crc32, bitpos, 4);
	crc32 = ENDSWAP_INT32(crc32);
	bitpos += sizeof(int);
	res = (crc32 != frame_crc32);

	bit_cache = bit_count = 0;
	frame_crc32 = 0xFFFFFFFFUL;

	return res;
}

/************************** WAV functions ******************************/

int read_wave(int *data, int byte_size, unsigned int len, FILE *fdin) {
	unsigned int res;
	unsigned char *src, *end;
	int *dst = data;

	src = WAVE_BUFFER;
	if (!(res = fread(WAVE_BUFFER, byte_size, len, fdin)))
		tta_error(READ_ERROR, NULL);
	end = WAVE_BUFFER + res * byte_size;

	switch (byte_size) {
	case 1: for (; src < end; dst++)
		*dst = (signed int) *src++ - 0x80;
		break;
	case 2: for (; src < end; dst++) {
		*dst = (unsigned char) *src++;
		*dst |= (signed char) *src++ << 8;
		}
		break;
	case 3: for (; src < end; dst++) {
		*dst = (unsigned char) *src++;
		*dst |= (unsigned char) *src++ << 8;
		*dst |= (signed char) *src++ << 16;
		}
		break;
	}

	return res;
}

int
write_wave(int *data, int byte_size, int num_chan, unsigned int len, FILE *fdout) {
	unsigned int res;
	int *src = data, *end;
	unsigned char *dst;

	dst = WAVE_BUFFER;
	end = data + len;

	switch (byte_size) {
	case 1: for (; src < end; src++)
		*dst++ = (unsigned char) (*src + 0x80);
		break;
	case 2: for (; src < end; src++) {
		*dst++ = (unsigned char) *src;
		*dst++ = (unsigned char) (*src >> 8);
		}
		break;
	case 3: for (; src < end; src++) {
		*dst++ = (unsigned char) *src;
		*dst++ = (unsigned char) (*src >> 8);
		*dst++ = (unsigned char) (*src >> 16);
		}
		break;
	}

	if (!(res = fwrite(WAVE_BUFFER, byte_size, len, fdout)))
		tta_error(WRITE_ERROR, NULL);

	return res;
}

/************************* filter functions ****************************/

__inline void memshl (register int *pA, register int *pB) {
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA++ = *pB++;
	*pA   = *pB;
}

__inline void hybrid_filter (fltst *fs, int *in, int mode) {
	register int *pA = fs->dl;
	register int *pB = fs->qm;
	register int *pM = fs->dx;
	register int sum = fs->round;

	if (!fs->error) {
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++;
		sum += *pA++ * *pB, pB++; pM += 8;
	} else if (fs->error < 0) {
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
		sum += *pA++ * (*pB -= *pM++), pB++;
	} else {
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
		sum += *pA++ * (*pB += *pM++), pB++;
	}

	*(pM-0) = ((*(pA-1) >> 30) | 1) << 2;
	*(pM-1) = ((*(pA-2) >> 30) | 1) << 1;
	*(pM-2) = ((*(pA-3) >> 30) | 1) << 1;
	*(pM-3) = ((*(pA-4) >> 30) | 1);

	if (mode) {
		*pA = *in;
		*in -= (sum >> fs->shift);
		fs->error = *in;
	} else {
		fs->error = *in;
		*in += (sum >> fs->shift);
		*pA = *in;
	}

	*(pA-1) = *(pA-0) - *(pA-1);
	*(pA-2) = *(pA-1) - *(pA-2);
	*(pA-3) = *(pA-2) - *(pA-3);

	memshl (fs->dl, fs->dl + 1);
	memshl (fs->dx, fs->dx + 1);
}

void filter_init (fltst *fs, int shift) {
	memset (fs, 0, sizeof(fltst));
	fs->shift = shift;
	fs->round = 1 << (shift - 1);
}

/************************* basic functions *****************************/

void rice_init(adapt *rice, unsigned int k0, unsigned int k1) {
	rice->k0 = k0;
	rice->k1 = k1;
	rice->sum0 = shift_16[k0];
	rice->sum1 = shift_16[k1];
}

void encoder_init(encoder *tta, int nch, int byte_size) {
	int flt_set [3] = { 10, 9, 10 };
	int i;

	for (i = 0; i < nch; i++) {
		filter_init(&tta[i].fst, flt_set[byte_size - 1]);
		rice_init(&tta[i].rice, 10, 10);
		tta[i].last = 0;
	}
}

int compress(FILE *fdin, FILE *fdout) 
{
	int *p, *data, tmp, prev;
	unsigned int num_chan, data_size, byte_size, data_len;
	unsigned int buffer_len, framelen, lastlen, fframes;
	unsigned int value, k, unary, binary;
	unsigned int st_size, *st, offset = 0;
	unsigned int def_subchunk_size = 16;
	encoder *tta, *enc;
	time_t stime = time(NULL);

	// clear statistics
	input_byte_count = output_byte_count = 0;

	// print process banner
	fwprintf(stderr, L"Encode:\t..\r");

	// copy ID3V2 header if present
	if (!fread(&id3v2, sizeof(id3v2), 1, fdin))
		tta_error(READ_ERROR, NULL);

	if (!memcmp(id3v2.id, "ID3", 3)) {
		char buffer[512];

		if (id3v2.size[0] & 0x80) {
			tta_error(FILE_ERROR, NULL);
			return 1;
		}

		offset = (id3v2.size[0] & 0x7f);
		offset = (offset << 7) | (id3v2.size[1] & 0x7f);
		offset = (offset << 7) | (id3v2.size[2] & 0x7f);
		offset = (offset << 7) | (id3v2.size[3] & 0x7f);
		if (id3v2.flags & (1 << 4)) offset += 10;
		data_len = offset, offset += 10;

		// write ID3V2 header
		if (!fwrite(&id3v2, sizeof(id3v2), 1, fdout))
			tta_error(WRITE_ERROR, NULL);

		while (data_len > 0) {
			int len = (data_len > sizeof(buffer))? sizeof(buffer):data_len;
			if (!fread(buffer, len, 1, fdin)) tta_error(READ_ERROR, NULL);
			if (!fwrite(buffer, len, 1, fdout)) tta_error(WRITE_ERROR, NULL);
			input_byte_count += len;
			output_byte_count += len;
			data_len -= len;
		}

		input_byte_count = output_byte_count = offset;
	} else {
		fseek(fdin, 0, SEEK_SET);
		if (ferror(fdin)) tta_error(READ_ERROR, NULL);
	}

	// read WAVE header
	if (!fread(&wave_hdr, sizeof(wave_hdr), 1, fdin))
		tta_error(READ_ERROR, NULL);
	input_byte_count += sizeof(wave_hdr);

	wave_hdr.ChunkID = ENDSWAP_INT32(wave_hdr.ChunkID);
	wave_hdr.ChunkSize = ENDSWAP_INT32(wave_hdr.ChunkSize);
	wave_hdr.Format = ENDSWAP_INT32(wave_hdr.Format);
	wave_hdr.Subchunk1ID = ENDSWAP_INT32(wave_hdr.Subchunk1ID);
	wave_hdr.Subchunk1Size = ENDSWAP_INT32(wave_hdr.Subchunk1Size);
	wave_hdr.AudioFormat = ENDSWAP_INT16(wave_hdr.AudioFormat);
	wave_hdr.NumChannels = ENDSWAP_INT16(wave_hdr.NumChannels);
	wave_hdr.SampleRate = ENDSWAP_INT32(wave_hdr.SampleRate);
	wave_hdr.ByteRate = ENDSWAP_INT32(wave_hdr.ByteRate);
	wave_hdr.BlockAlign = ENDSWAP_INT16(wave_hdr.BlockAlign);
	wave_hdr.BitsPerSample = ENDSWAP_INT16(wave_hdr.BitsPerSample);

	// check for supported formats
	if ((wave_hdr.ChunkID != RIFF_SIGN) ||
		(wave_hdr.Format != WAVE_SIGN) ||
		(wave_hdr.Subchunk1ID != fmt_SIGN) ||
		(wave_hdr.Subchunk1Size > wave_hdr.ChunkSize) ||
		(wave_hdr.NumChannels == 0) ||
		(wave_hdr.BitsPerSample > MAX_BPS)) {
		
		tta_error(FORMAT_ERROR, NULL);
		return 1;
	}

	if (wave_hdr.AudioFormat == WAVE_FORMAT_EXTENSIBLE) {
		EXTENSIBLE_WAV_HDR wave_hdr_ex;

		if (!fread(&wave_hdr_ex, sizeof(wave_hdr_ex), 1, fdin))
			tta_error(READ_ERROR, NULL);
		input_byte_count += sizeof(wave_hdr_ex);

		def_subchunk_size += sizeof(wave_hdr_ex);
		wave_hdr.AudioFormat = ENDSWAP_INT32(wave_hdr_ex.est.f1);
	}

	if (wave_hdr.AudioFormat != WAVE_FORMAT_PCM ||
	wave_hdr.BitsPerSample > MAX_BPS) {
		tta_error(FORMAT_ERROR, NULL);
		return 1;
	}
	
	// skip extra format bytes
	if (wave_hdr.Subchunk1Size > def_subchunk_size) {
		unsigned int extra_len = wave_hdr.Subchunk1Size - def_subchunk_size;

		fseek(fdin, extra_len, SEEK_CUR);
		if (ferror(fdin)) tta_error(READ_ERROR, NULL);
		input_byte_count += extra_len;

		fwprintf(stderr, L"Encode:\tskiped %ld extra format bytes\n", extra_len);
	}

	// skip unsupported chunks
	while (1) {
		char chunk_id[5];

		if (!fread(&subchunk_hdr, sizeof(subchunk_hdr), 1, fdin))
			tta_error(READ_ERROR, NULL);
		input_byte_count += sizeof(subchunk_hdr);

		subchunk_hdr.SubchunkSize = ENDSWAP_INT32(subchunk_hdr.SubchunkSize);
		subchunk_hdr.SubchunkID = ENDSWAP_INT32(subchunk_hdr.SubchunkID);
		if (subchunk_hdr.SubchunkID == data_SIGN) break;

		if (subchunk_hdr.SubchunkSize & 0x80000000UL) {
			tta_error(FILE_ERROR, NULL);
			return 1;
		}

		fseek(fdin, subchunk_hdr.SubchunkSize, SEEK_CUR);
		if (ferror(fdin)) tta_error(READ_ERROR, NULL);
		input_byte_count += subchunk_hdr.SubchunkSize;

		memcpy(chunk_id, &subchunk_hdr.SubchunkID, 4);
		chunk_id[4] = 0;

		fwprintf(stderr, L"Encode:\tskiped unsupported '%hs' chunk\n", chunk_id);
	}

	framelen = (int) (FRAME_TIME * wave_hdr.SampleRate);
	num_chan = wave_hdr.NumChannels;
	data_size = subchunk_hdr.SubchunkSize;
	byte_size = (wave_hdr.BitsPerSample + 7) / 8;
	data_len = data_size / (byte_size * num_chan);

	lastlen = data_len % framelen;
	fframes = data_len / framelen + (lastlen ? 1 : 0);
	st_size = (fframes + 1);
	buffer_len = num_chan * framelen;

	tta_hdr.TTAid = ENDSWAP_INT32(TTA1_SIGN);
	tta_hdr.AudioFormat = ENDSWAP_INT16(wave_hdr.AudioFormat); 
	tta_hdr.NumChannels = ENDSWAP_INT16(wave_hdr.NumChannels);
	tta_hdr.BitsPerSample = ENDSWAP_INT16(wave_hdr.BitsPerSample);
	tta_hdr.SampleRate = ENDSWAP_INT32(wave_hdr.SampleRate);
	tta_hdr.DataLength = ENDSWAP_INT32(data_len);
	tta_hdr.CRC32 = crc32((unsigned char *) &tta_hdr,
			  sizeof(tta_hdr) - sizeof(int));
	tta_hdr.CRC32 = ENDSWAP_INT32(tta_hdr.CRC32);

	// grab some space for an encoder buffers
	data = (int *) tta_malloc(buffer_len, sizeof(int));
	st = seek_table = (unsigned int *) tta_malloc(st_size, sizeof(int));
	enc = tta = tta_malloc(num_chan, sizeof(encoder));
	WAVE_BUFFER = (unsigned char *) tta_malloc(buffer_len, byte_size);

	// write TTA header
	if (!fwrite(&tta_hdr, sizeof(tta_hdr), 1, fdout))
		tta_error(WRITE_ERROR, NULL);
	else output_byte_count += sizeof(tta_hdr);

	// allocate space for a seek table
	if (!fwrite(seek_table, st_size, sizeof(int), fdout))
		tta_error(WRITE_ERROR, NULL);
	else output_byte_count += st_size * sizeof(int);

	// init bit writer
	init_buffer_write(output_byte_count);

	while (fframes--) {
		if (!fframes && lastlen)
			buffer_len = num_chan * (framelen = lastlen);

		read_wave(data, byte_size, buffer_len, fdin);
		encoder_init(tta, num_chan, byte_size);

		for (p = data, prev = 0; p < data + buffer_len; p++) {
			fltst *fst = &enc->fst;
			adapt *rice = &enc->rice;
			int *last = &enc->last;

			// transform data
			if (enc < tta + num_chan - 1)
				*p = prev = *(p + 1) - *p;
			else *p -= prev / 2;

			// compress stage 1: fixed order 1 prediction
			tmp = *p;
			switch (byte_size) {
			case 1:	*p -= PREDICTOR1(*last, 4); break;	// bps 8
			case 2:	*p -= PREDICTOR1(*last, 5); break;	// bps 16
			case 3:	*p -= PREDICTOR1(*last, 5); break;	// bps 24
			} *last = tmp;

			// compress stage 2: adaptive hybrid filter
			hybrid_filter(fst, p, 1);

			value = ENC(*p);

			// encode Rice unsigned
			k = rice->k0;

			rice->sum0 += value - (rice->sum0 >> 4);
			if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
				rice->k0--;
			else if (rice->sum0 > shift_16[rice->k0 + 1])
				rice->k0++;

			if (value >= bit_shift[k]) {
				value -= bit_shift[k];
				k = rice->k1;

				rice->sum1 += value - (rice->sum1 >> 4);
				if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
					rice->k1--;
				else if (rice->sum1 > shift_16[rice->k1 + 1])
					rice->k1++;

				unary = 1 + (value >> k);
			} else unary = 0;

			put_unary(unary);
			if (k) {
				binary = value & bit_mask[k];
				put_binary(binary, k);
			}

			if (enc < tta + num_chan - 1) enc++;
			else enc = tta;
		}

		*st++ = done_buffer_write();

		input_byte_count += byte_size * buffer_len;
	}

	// update the seek table
	fseek(fdout, sizeof(tta_hdr) + offset, SEEK_SET);
	if (ferror(fdout)) tta_error(WRITE_ERROR, NULL);

	for (st = seek_table; st < (seek_table + st_size - 1); st++)
		*st = ENDSWAP_INT32(*st);
	seek_table[st_size - 1] = crc32((unsigned char *) seek_table, 
		(st_size - 1) * sizeof(int));
	seek_table[st_size - 1] = ENDSWAP_INT32(seek_table[st_size - 1]);
	if (!fwrite(seek_table, st_size, sizeof(int), fdout))
		tta_error(WRITE_ERROR, NULL);

	free(WAVE_BUFFER);
	free(seek_table);
	free(data);
	free(tta);

	fwprintf(stderr, L"Encode:\tcomplete, wrote %ld bytes, ratio: %.2f, time: %d\n",
		(int) output_byte_count,
		(float) output_byte_count / (input_byte_count + 1),
		(int) (time(NULL) - stime));
	fwprintf(stderr, L"%hs\n", LINE);

	return 0;
}

int test_file(FILE *fdin) {
	unsigned int byte_size, data_size, checksum, errors;
	unsigned int framelen, lastlen, fframes;
	unsigned int framesize, st_size, *st;
	unsigned char *data;

	// clear statistics
	input_byte_count = output_byte_count = 0;

	// print process banner
	fwprintf(stderr, L"Test:\t..\r");

	// skip ID3V2 header
	if (!fread(&id3v2, sizeof(id3v2), 1, fdin))
		tta_error(READ_ERROR, NULL);

	if (!memcmp(id3v2.id, "ID3", 3)) {
		int len;

		if (id3v2.size[0] & 0x80) {
			fwprintf(stderr, L"Error:\tID3 header is corrupted\n");
			return 1;
		}

		len = (id3v2.size[0] & 0x7f);
		len = (len << 7) | (id3v2.size[1] & 0x7f);
		len = (len << 7) | (id3v2.size[2] & 0x7f);
		len = (len << 7) | (id3v2.size[3] & 0x7f);
		len += 10;
		if (id3v2.flags & (1 << 4)) len += 10;

		fseek(fdin, len, SEEK_SET);
		input_byte_count += len;
	} else fseek(fdin, 0, SEEK_SET);

	// read TTA header
	if (!fread(&tta_hdr, sizeof(tta_hdr), 1, fdin))
		tta_error(READ_ERROR, NULL);
	else input_byte_count += sizeof(tta_hdr);

	// check for supported formats
	if (ENDSWAP_INT32(tta_hdr.TTAid) != TTA1_SIGN) {
		fwprintf(stderr, L"Error:\tTTA ID is not found\n");
		return 1;
	}

	tta_hdr.AudioFormat = ENDSWAP_INT16(tta_hdr.AudioFormat); 
	tta_hdr.NumChannels = ENDSWAP_INT16(tta_hdr.NumChannels);
	tta_hdr.BitsPerSample = ENDSWAP_INT16(tta_hdr.BitsPerSample);
	tta_hdr.SampleRate = ENDSWAP_INT32(tta_hdr.SampleRate);
	tta_hdr.DataLength = ENDSWAP_INT32(tta_hdr.DataLength);

	tta_hdr.CRC32 = ENDSWAP_INT32(tta_hdr.CRC32);
	checksum = crc32((unsigned char *) &tta_hdr,
		sizeof(tta_hdr) - sizeof(int));
	if (checksum != tta_hdr.CRC32) {
		fwprintf(stderr, L"Error:\tHeader checksum failed\n");
		return 1;
	}

	byte_size = (tta_hdr.BitsPerSample + 7) / 8;
	framelen = (int) (FRAME_TIME * tta_hdr.SampleRate);
	data_size = tta_hdr.DataLength * byte_size * tta_hdr.NumChannels;
	framesize = framelen * tta_hdr.NumChannels * byte_size + 4;
	lastlen = tta_hdr.DataLength % framelen;
	fframes = tta_hdr.DataLength / framelen + (lastlen ? 1 : 0);
	st_size = (fframes + 1);

	// grab some space for a buffer
	data = (unsigned char *) tta_malloc(framesize, 1);
	seek_table = (unsigned int *) tta_malloc(st_size, sizeof(int));

	// read seek table
	if (!fread(seek_table, st_size, sizeof(int), fdin))
		tta_error(READ_ERROR, NULL);
	else input_byte_count += st_size * sizeof(int);

	checksum = crc32((unsigned char *) seek_table, 
		(st_size - 1) * sizeof(int));
	if (checksum != ENDSWAP_INT32(seek_table[st_size - 1])) {
		fwprintf(stderr, L"Error:\tseek table corrupted\n");
		free(seek_table);
		free(data);
		return 1;
	}

	// check frames
	for (st = seek_table, errors = 0;
		st < (seek_table + st_size - 1); st++) {
		int ret = 0;

		*st = ENDSWAP_INT32(*st);
		ret = fread(data, 1, *st, fdin);
		if (!ret) tta_error(READ_ERROR, NULL);
		input_byte_count += ret;

		memcpy(&frame_crc32, data + (ret - 4), 4);
		checksum = crc32(data, *st - 4);

		if (checksum != ENDSWAP_INT32(frame_crc32))
			errors++;
	}

	free(seek_table);
	free(data);

	if (errors) {
		fwprintf(stderr, L"Test:\tfailed, %d frame(s) corrupted\n", (int)errors);
		return 1;
	}

	fwprintf(stderr, L"Test:\tcomplete\n");

	return 0;
}

int decompress(FILE *fdin, FILE *fdout) {
	int *p, *data, value;
	unsigned int num_chan, data_size, byte_size, checksum;
	unsigned int buffer_len, framelen, lastlen, fframes;
	unsigned int k, depth, unary, binary = 0;
	unsigned int st_size, st_state, *st;
	unsigned int def_subchunk_size = 16;
	encoder *tta, *enc;
	time_t stime = time(NULL);

	// clear statistics
	input_byte_count = output_byte_count = 0;

	// print process banner
	fwprintf(stderr, L"Decode:\t..\r");

	// skip ID3V2 header
	if (!fread(&id3v2, sizeof(id3v2), 1, fdin))
		tta_error(READ_ERROR, NULL);
	if (!memcmp(id3v2.id, "ID3", 3)) {
		int len;

		if (id3v2.size[0] & 0x80) {
			tta_error(FILE_ERROR, NULL);
			return 1;
		}

		len = (id3v2.size[0] & 0x7f);
		len = (len << 7) | (id3v2.size[1] & 0x7f);
		len = (len << 7) | (id3v2.size[2] & 0x7f);
		len = (len << 7) | (id3v2.size[3] & 0x7f);
		len += 10;
		if (id3v2.flags & (1 << 4)) len += 10;

		fseek(fdin, len, SEEK_SET);
		input_byte_count += len;
	} else fseek(fdin, 0, SEEK_SET);

	// read TTA header
	if (!fread(&tta_hdr, sizeof(tta_hdr), 1, fdin))
		tta_error(READ_ERROR, NULL);
	else input_byte_count += sizeof(tta_hdr);

	// check for supported formats
	if (ENDSWAP_INT32(tta_hdr.TTAid) != TTA1_SIGN) {
		tta_error(FORMAT_ERROR, NULL);
		return 1;
	}

	tta_hdr.CRC32 = ENDSWAP_INT32(tta_hdr.CRC32);
	checksum = crc32((unsigned char *) &tta_hdr,
		sizeof(tta_hdr) - sizeof(int));
	if (checksum != tta_hdr.CRC32) {
		tta_error(FILE_ERROR, NULL);
		return 1;
	}

	tta_hdr.AudioFormat = ENDSWAP_INT16(tta_hdr.AudioFormat); 
	tta_hdr.NumChannels = ENDSWAP_INT16(tta_hdr.NumChannels);
	tta_hdr.BitsPerSample = ENDSWAP_INT16(tta_hdr.BitsPerSample);
	tta_hdr.SampleRate = ENDSWAP_INT32(tta_hdr.SampleRate);
	tta_hdr.DataLength = ENDSWAP_INT32(tta_hdr.DataLength);

	if (tta_hdr.AudioFormat != WAVE_FORMAT_PCM ||
	tta_hdr.BitsPerSample > MAX_BPS) {
		tta_error(FORMAT_ERROR, NULL);
		return 1;
	}

	byte_size = (tta_hdr.BitsPerSample + 7) / 8;
	framelen = (int) (FRAME_TIME * tta_hdr.SampleRate);
	num_chan = tta_hdr.NumChannels;
	data_size = tta_hdr.DataLength * byte_size * num_chan;

	if (wave_ext) {
		def_subchunk_size += sizeof(EXTENSIBLE_WAV_HDR);
		wave_hdr.AudioFormat = ENDSWAP_INT16(WAVE_FORMAT_EXTENSIBLE);
	} else wave_hdr.AudioFormat = ENDSWAP_INT16(tta_hdr.AudioFormat);

	wave_hdr.ChunkID = ENDSWAP_INT32(RIFF_SIGN);
	wave_hdr.ChunkSize = ENDSWAP_INT32(data_size + 36);
	wave_hdr.Format = ENDSWAP_INT32(WAVE_SIGN);
	wave_hdr.Subchunk1ID = ENDSWAP_INT32(fmt_SIGN);
	wave_hdr.Subchunk1Size = ENDSWAP_INT32(def_subchunk_size);
	wave_hdr.NumChannels = ENDSWAP_INT16((unsigned short) num_chan);
	wave_hdr.SampleRate = ENDSWAP_INT32(tta_hdr.SampleRate);
	wave_hdr.BitsPerSample = ENDSWAP_INT16(tta_hdr.BitsPerSample);
	wave_hdr.ByteRate = tta_hdr.SampleRate * byte_size * num_chan;
	wave_hdr.ByteRate = ENDSWAP_INT32(wave_hdr.ByteRate);
	wave_hdr.BlockAlign = (unsigned short) (num_chan * byte_size);
	wave_hdr.BlockAlign = ENDSWAP_INT16(wave_hdr.BlockAlign);
	subchunk_hdr.SubchunkID = ENDSWAP_INT32(data_SIGN);
	subchunk_hdr.SubchunkSize = ENDSWAP_INT32(data_size);

	lastlen = tta_hdr.DataLength % framelen;
	fframes = tta_hdr.DataLength / framelen + (lastlen ? 1 : 0);
	st_size = (fframes + 1);
	st_state = 0;
	buffer_len = num_chan * framelen;

	// grab some space for a buffer
	data = (int *) tta_malloc(buffer_len, sizeof(int));
	enc = tta = tta_malloc(num_chan, sizeof(encoder));
	seek_table = (unsigned int *) tta_malloc(st_size, sizeof(int));
	WAVE_BUFFER = (unsigned char *) tta_malloc(buffer_len, byte_size);

	// read seek table
	if (!fread(seek_table, st_size, sizeof(int), fdin))
		tta_error(READ_ERROR, NULL);
	else input_byte_count += st_size * sizeof(int);

	checksum = crc32((unsigned char *) seek_table, 
		(st_size - 1) * sizeof(int));
	if (checksum != ENDSWAP_INT32(seek_table[st_size - 1]))
		fwprintf(stderr, L"Decode:\twarning, seek table corrupted\n");
	else st_state = 1;

	for (st = seek_table; st < (seek_table + st_size); st++)
		*st = ENDSWAP_INT32(*st);

	// write WAVE header
	if (!fwrite(&wave_hdr, sizeof(wave_hdr), 1, fdout))
		tta_error(WRITE_ERROR, NULL);
	else output_byte_count += sizeof(wave_hdr);
	
	if (wave_ext) {
		EXTENSIBLE_WAV_HDR wave_hdr_ex;
		unsigned int chMask = 0;

		switch (tta_hdr.NumChannels) {
		case 2: chMask = 0x00000003; break;
		case 3: chMask = 0x0000000B; break;
		case 4: chMask = 0x00000033; break;
		case 6: chMask = 0x0000003F; break;
		case 7: chMask = 0x0000013F; break;
		case 8: chMask = 0x000000FF; break;
		};

		wave_hdr_ex.cbSize = ENDSWAP_INT16(22);
		wave_hdr_ex.validBits = ENDSWAP_INT16(wave_hdr.BitsPerSample);
		wave_hdr_ex.chMask = ENDSWAP_INT32(chMask);
		wave_hdr_ex.est.f1 = ENDSWAP_INT32(tta_hdr.AudioFormat);
		wave_hdr_ex.est.f2 = 0;
		wave_hdr_ex.est.f3 = ENDSWAP_INT16(0x10);
		wave_hdr_ex.est.f4[0] = 0x80;
		wave_hdr_ex.est.f4[1] = 0x00;
		wave_hdr_ex.est.f4[2] = 0x00;
		wave_hdr_ex.est.f4[3] = 0xaa;
		wave_hdr_ex.est.f4[4] = 0x00;
		wave_hdr_ex.est.f4[5] = 0x38;
		wave_hdr_ex.est.f4[6] = 0x9b;
		wave_hdr_ex.est.f4[7] = 0x71;

		if (!fwrite(&wave_hdr_ex, sizeof(wave_hdr_ex), 1, fdout))
			tta_error(WRITE_ERROR, NULL);
		else output_byte_count += sizeof(wave_hdr_ex);
	}
	
	// write Subchunk header
	if (!fwrite(&subchunk_hdr, sizeof(subchunk_hdr), 1, fdout))
		tta_error(WRITE_ERROR, NULL);
	else output_byte_count += sizeof(subchunk_hdr);

	// init bit reader
	init_buffer_read(input_byte_count);

	st = seek_table;
	while (fframes--) {
		if (!fframes && lastlen)
			buffer_len = num_chan * (framelen = lastlen);

		encoder_init(tta, num_chan, byte_size);
		for (p = data; p < data + buffer_len; p++) {
			fltst *fst = &enc->fst;
			adapt *rice = &enc->rice;
			int *last = &enc->last;

			// decode Rice unsigned
			get_unary(&unary);

			switch (unary) {
			case 0: depth = 0; k = rice->k0; break;
			default:
				depth = 1; k = rice->k1;
				unary--;
			}

			if (k) {
				get_binary(&binary, k);
				value = (unary << k) + binary;
			} else value = unary;

			switch (depth) {
			case 1: 
				rice->sum1 += value - (rice->sum1 >> 4);
				if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
					rice->k1--;
				else if (rice->sum1 > shift_16[rice->k1 + 1])
					rice->k1++;
				value += bit_shift[rice->k0];
			default:
				rice->sum0 += value - (rice->sum0 >> 4);
				if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
					rice->k0--;
				else if (rice->sum0 > shift_16[rice->k0 + 1])
					rice->k0++;
			}

			*p = DEC(value);

			// decompress stage 1: adaptive hybrid filter
			hybrid_filter(fst, p, 0);

			// decompress stage 2: fixed order 1 prediction
			switch (byte_size) {
			case 1: *p += PREDICTOR1(*last, 4); break;	// bps 8
			case 2: *p += PREDICTOR1(*last, 5); break;	// bps 16
			case 3: *p += PREDICTOR1(*last, 5); break;	// bps 24
			} *last = *p;

			if (enc < tta + num_chan - 1) enc++;
			else {
				if (num_chan > 1) {
					int *r = p - 1;
					for (*p += *r/2; r > p - num_chan; r--)
						*r = *(r + 1) - *r;
				}
				enc = tta;
			}
		}

		lastpos += *st++;

		if (done_buffer_read()) {
			if (st_state) {
				ERASE_STDERR;
				fwprintf(stderr, L"Decode:\tchecksum error, %ld samples wiped\n", framelen);
				memset(data, 0, buffer_len * sizeof(int));
				fseek(fdin, lastpos, SEEK_SET);
				init_buffer_read(lastpos);
			} else {
				tta_error(FILE_ERROR, NULL);
				goto done;
			}
			fflush(stderr);
		}

		output_byte_count +=
			write_wave(data, byte_size, num_chan, buffer_len, fdout) * byte_size;
	}

done:

	free(WAVE_BUFFER);
	free(seek_table);
	free(data);
	free(tta);

	fwprintf(stderr, L"Decode:\tcomplete, wrote %ld bytes, ratio: %.2f, time: %d\n",
		(int) (output_byte_count),
		(float) output_byte_count / (input_byte_count + 1),
		(int) (time(NULL) - stime));
	fwprintf(stderr, L"%hs\n", LINE);

	return 0;
}

void path_strcat(wchar_t *pstr1, wchar_t *pstr2) {
	int len;

	len = wcslen(pstr1);
	if (len && (pstr1[len] != _SEP))
		pstr1[len++] = _SEP;

	wcsncat(pstr1, pstr2, _MAX_FNAME - len - 1);
}

void add_to_files_list(wchar_t *path, struct flist **head, struct flist **tail) {
	struct flist *new_item;

	new_item = (struct flist *) tta_malloc(1, sizeof(struct flist));
	wcscpy(new_item->fname, path);
	new_item->next = NULL;

	if (*head == NULL) *head = *tail = new_item;
	else *tail = (*tail)->next = new_item;
}

void clear_files_list(void) {
	struct flist *item;

	while (files_list != NULL) {
		item = files_list;
		files_list = files_list->next;
		free(item);
	}
}

void usage(void) {
	fwprintf(stderr, L"usage:\t%hs [command] [options] file(s).. <output path/>\n\n", MYNAME);
	fwprintf(stderr, L"%hs\n", LINE);
	fwprintf(stderr, L"commands:\n");
	fwprintf(stderr, L"%hs\n", LINE);
	fwprintf(stderr, L"\t-e\tencode file(s)\n");
	fwprintf(stderr, L"\t-d\tdecode file(s)\n");
	fwprintf(stderr, L"\t-t\ttest file(s)\n");
	fwprintf(stderr, L"\t-o name\tspecify output file name\n");
	fwprintf(stderr, L"\t-v\tshow codec version\n");
	fwprintf(stderr, L"\t-h\tthis help\n");
	fwprintf(stderr, L"%hs\n", LINE);
	fwprintf(stderr, L"options:\n");
	fwprintf(stderr, L"%hs\n", LINE);
	fwprintf(stderr, L"\t-x\twave-extensible output file format\n");
	fwprintf(stderr, L"\t-u\tdelete source file if successful\n");
	fwprintf(stderr, L"%hs\n", LINE);
	fwprintf(stderr, L"when file is '-', use standard input/output.\n\n");

	exit(0);
}

void fill_out_name_with_extention(wchar_t *ext) {
	int len;
	wchar_t *p, *filename;

	len = wcslen(file_out);
	if (len && (file_out[len] != _SEP))
		file_out[len++] = _SEP;

	filename = wcsrchr(file_in, _SEP);
	filename = (!filename) ? file_in : (filename + 1);

	wcscat(file_out, filename);
	p = wcsrchr(file_out, '.');
	if (p) *p = L'\0';

	wcscat(file_out, ext);
}

void process_files(int act, int files) {
	struct flist *item;
	time_t ftime = time(NULL);
	int count = 0;
	int processed = 0;
	int ret = 0;
#ifdef _MSC
	char status[256];
#endif

	for (item = files_list; item != NULL; item = item->next) {

#ifdef _MSC
		sprintf(status, "TTA: %d/%d - %d file(s) processed",
			count + 1, files, processed);
		SetConsoleTitle(status);
#endif

		memset(file_in, 0, sizeof(file_in));
		memset(file_out, 0, sizeof(file_out));

		wcscpy(file_in, item->fname);
		wcscpy(file_out, out_path);

		if (!fixed_out) switch (act) {
			case 1: fill_out_name_with_extention(L".tta"); break;
			case 2: fill_out_name_with_extention(L".wav"); break;
		}

		// print file banner
		fwprintf(stderr, L"File:\t[%ls]\n", print_path(file_in, 0));
		
		fdin = stdin;
		if (wcscmp(file_in, L"-")) fdin = wfopen(file_in, "rb");
#ifdef _WIN32
		else setmode(fileno(stdin), O_BINARY);
#endif
		if (!fdin) tta_error(OPENR_ERROR, file_in);

		fdout = stdout;
		if (wcscmp(file_out, L"-")) {
			if (!wcscmp(file_in, file_out)) fdout = NULL;
			else fdout = wfopen(file_out, "wb");
		}
#ifdef _WIN32
		else setmode(fileno(stdout), O_BINARY);
#endif
		if (!fdout) tta_error(OPENW_ERROR, file_out);

		switch (act) {
		case 1: ret = compress(fdin, fdout); break;
		case 2: ret = decompress(fdin, fdout); break;
		}

		if (wcscmp(file_in, L"-")) fclose(fdin);
		if (wcscmp(file_out, L"-")) fclose(fdout);

		if (!ret) {
			total_input_bytes += input_byte_count;
			total_output_bytes += output_byte_count;
			if (clean_src) wunlink(file_in);
			processed++;
		}

		count++;
	}

#ifdef _MSC
	sprintf(status, "TTA: %d/%d - %d file(s) processed",
		count, files, processed);
	SetConsoleTitle(status);
#endif

	ftime = (int) (time(NULL) - ftime);
	fwprintf(stderr, L"Total:\t[%d/%ld, %.1f/%.1f Mb], ratio: %.3f, time: %ld'%02ld\n",
		processed, files, (float) total_output_bytes / 1048576,
		(float) total_input_bytes / 1048576,
		(float) total_output_bytes / (total_input_bytes + 1),
		ftime / 60, ftime % 60);
	fwprintf(stderr, L"%hs\n\n", LINE);
}

void test_files(int act, int files) {
	struct flist *item;
	time_t ftime = time(NULL);
	int count = 0;
	int processed = 0;
	int ret = 0;
#ifdef _MSC
	char status[256];
#endif

	for (item = files_list; item != NULL; item = item->next) {

#ifdef _MSC
		sprintf(status, "TTA: %d/%d - %d file(s) processed",
			count + 1, files, processed);
		SetConsoleTitle(status);
#endif
		wcscpy(file_in, item->fname);

		fdin = stdin;
		if (wcscmp(file_in, L"-")) fdin = wfopen(file_in, "rb");
		if (!fdin) tta_error(OPENR_ERROR, file_in);

		// print file banner
		fwprintf(stderr, L"File:\t[%ls]\n", print_path(file_in, 0));

		ret = test_file(fdin);

		if (wcscmp(file_in, L"-")) fclose(fdin);
		if (!ret) processed++;

		count++;
	}

#ifdef _MSC
	sprintf(status, "TTA: %d/%d - %d file(s) processed",
		count, files, processed);
	SetConsoleTitle(status);
#endif

	ftime = (int) (time(NULL) - ftime);
	fwprintf(stderr, L"%hs\nTotal:\t[%d/%ld] succeeded, time: %ld'%02ld\n",
		LINE, processed, files, ftime / 60, ftime % 60);
	fwprintf(stderr, L"%hs\n\n", LINE);
}

/******************************* main **********************************/

int
#ifdef _MSC
__cdecl wmain(int argc, wchar_t **argv)
#else
main(int argc, char **argv)
#endif
{
	int i;
	unsigned int j;
	int farg = 0, parg = 0, act = 0;
	wchar_t *p;
#ifdef _MSC
	struct _wfinddata_t fdata;
	int hFile;
#else
	struct stat st;
	int ret;
#endif

#ifdef _MSC
	setlocale(LC_ALL, ".OCP");
#else
	setlocale(LC_ALL, "");
#endif

	fwprintf(stderr, L"TTA1 lossless audio encoder/decoder, release %hs\n%hs\n", VERSION, COPYRIGHT);
	fwprintf(stderr, L"For more information see %hs\n%hs\n", PROJECT_URL, LINE);

	total_input_bytes = total_output_bytes = 0;
	*out_path = *file_in = *file_out = L'\0';

	if (argc) {
		for (i = 1; i < argc; i++) {
			if (argv[i][0] == '-') {
				if (argv[i][1] == L'\0') {
					wcscpy(file_in, L"-");
					if (!fixed_out) {
						wcscpy(file_out, L"-");
						fixed_out = 1;
					}
					add_to_files_list(file_in, &files_list, &files_list_tail);
					farg++;
					continue;
				}
				for (j = 1; j < strlen(argv[i]); j++) {
					switch (argv[i][j]) {
					case 'e':
						act = 1;
						break;
					case 'd':
						act = 2;
						break;
					case 'x':
						wave_ext = 1;
						break;
					case 't':
						act = 3;
						break;
					case 'u':
						clean_src = 1;
						break;
					case 'o':
						fixed_out = 1;
						i++;
						wstrncpy(file_out, argv[i], _MAX_FNAME-1);
						goto next;
					case 'v':
						fwprintf(stderr, L"%hs: version %hs build %hs\n\n", MYNAME, VERSION, BUILD);
						exit(0);
					case 'h':
						usage();
					default:
						wstrncpy(file_in, argv[i], _MAX_FNAME-1);
						tta_error(COMMAND_ERROR, file_in);
						usage();
					}
				}
next:				continue;
			}
			else
			{
				wstrncpy(file_in, argv[i], _MAX_FNAME-1);

				if (!act && (p = wcsrchr(file_in, '.'))) {
					if (!wcsicmp(p, L".wav")) act = 1;
					if (!wcsicmp(p, L".tta")) act = 2;
				}
#ifdef _MSC
				if ((hFile = _wfindfirst(file_in, &fdata)) == -1) {
					if (i == argc - 1 && farg) {
						wcscpy(out_path, file_in);
						parg = 1;
						if (_wmkdir(out_path) && errno != EEXIST)
							tta_error(CREATE_ERROR, out_path);
						continue;
					} else tta_error(FIND_ERROR, file_in);
				}

				switch (fdata.attrib) {
				case _A_SUBDIR:
					if (i == argc - 1 && farg) {
						wcscpy(out_path, file_in);
						parg = 1;
					} else tta_error(FIND_ERROR, file_in);
					break;
				default:
					p = wcsrchr(file_in, _SEP);
					p = (!p) ? file_in : (p + 1);
					*p = L'\0';
					wcscat(file_in, fdata.name);
					add_to_files_list(file_in, &files_list, &files_list_tail);
					farg++;
					while (_wfindnext(hFile, &fdata) == 0) {
						*p = L'\0';
						wcscat(file_in, fdata.name);
						add_to_files_list(file_in, &files_list, &files_list_tail);
						farg++;
					}
				}
				_findclose(hFile);

#else
				ret = stat(argv[i], &st);
				if (ret && errno == ENOENT) {
					if (i == argc - 1 && farg) {
						wcscpy(out_path, file_in);
						parg = 1;
						if (mkdir(argv[i], S_IRUSR | S_IWUSR | S_IXUSR))
							tta_error(CREATE_ERROR, out_path);
						continue;
					} else tta_error(FIND_ERROR, file_in);
				}

				if (!ret && S_ISDIR(st.st_mode)) {
					if (i == argc - 1 && farg) {
						wcscpy(out_path, file_in);
						parg = 1;
						continue;
					} else tta_error(FIND_ERROR, file_in);
				}

				add_to_files_list(file_in, &files_list, &files_list_tail);
				farg++;
#endif
			}
		}
	} else usage();

	if (!act || !farg) usage();
	if (act == 3) {
		test_files(act, farg);
		goto done;
	}

	if (fixed_out)
		path_strcat(out_path, file_out);
	process_files(act, farg);
done:
	clear_files_list();
	return 0;
}

/* eof */

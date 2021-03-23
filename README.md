TTA Lossless Audio compressor
===============================

*Originally this is copied from http://downloads.sourceforge.net/tta/ttaenc-3.4.1-src.tgz*


TTA performs lossless compression on multichannel 8,16 and 24 bits
data of the Wav audio files. Being "lossless" means that no data-
quality is lost in the compression - when uncompressed, the data will
be identical to the original. The compression ratios of TTA depend on
the type of music file being compressed, but the compression size
will generally range between 30% - 70% of the original. TTA format
supports both of ID3v1/v2 and APEv2 tags. Detailed format description
is available at http://tta.sourceforge.net

Usage Instructions
------------------
```
ttaenc [command] [options] file(s).. <output path/>

Commands:

  -e      encode file(s)
  -d      decode file(s)
  -t      test file(s)
  -f name specify output file name
  -v      show codec version
  -h      this help

Options:

  -u      delete source file if successful
  -x      wave-extensible output file format

When file is '-', use standard input/output.
Example usage:

  ttaenc *.wav; ttaenc file.tta /audio
  ttaenc file.tta -do file.wav
  ttaenc -e - < file.wav > file.tta
  ttaenc - < file.tta -do file.wav
  ttaenc file.tta -do - > file.wav
  ttaenc -e *.wav; ttaenc -d *.tta /audio
```
When using wildcards, only the input name can be specified. The output
names will be automatically determined as the original name with the new
extension.

Features
--------

  - Support of 8,16,24 integer,
    multichannel Wav audio files;
  - Support unicode file names;
  - Minimal system requirements;
  - High compression performance;
  - High speed performance;
  - CRC checking;
  - Corrupted data decoding;
  - Simple and clean output files format with
    precalculated seek points table.

Contributors:
-------------

  Aleksander Djuric
  Pavel Zhilin
  Tamir Barak
  Noam Koenigstein
  Kazuki Oikawa

Copying
-------

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Please see the file COPYING in this directory for full copyright
information.

See also
--------

Please visit the TTA homepage at http://tta.sourceforge.net for the
latest in news and downloads.

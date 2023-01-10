(******************************************************************************)
(*                               libVorbis_dyn                                *)
(*                  free pascal wrapper around Vorbis library                 *)
(*                          https://xiph.org/vorbis/                          *)
(*                                                                            *)
(* Copyright (c) 2022 Ilya Medvedkov                                          *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the  GNU Lesser General Public License  as published by *)
(* the Free Software Foundation; either version 3 of the License (LGPL v3).   *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.                                          *)
(* See the GNU Lesser General Public License for more details.                *)
(*                                                                            *)
(* A copy of the GNU Lesser General Public License is available on the World  *)
(* Wide Web at <https://www.gnu.org/licenses/lgpl-3.0.html>.                  *)
(*                                                                            *)
(******************************************************************************)

unit libVorbis_dyn;

{$mode objfpc}{$H+}

{$packrecords c}

interface

uses dynlibs, SysUtils, libOGG_dynlite, ctypes;

const
{$if defined(UNIX) and not defined(darwin)}
  VorbisDLL: Array [0..2] of string = ('libvorbis.so',
                                       'libvorbisenc.so',
                                       'libvorbisfile.so');
{$ELSE}
{$ifdef WINDOWS}
  VorbisDLL: Array [0..2] of string = ('vorbis.dll',
                                       'vorbisenc.dll',
                                       'vorbisfile.dll');
{$endif}
{$endif}

type
  pFILE = pointer;
  ppcfloat = ^pcfloat;
  pppcfloat = ^ppcfloat;

  pvorbis_info = ^vorbis_info;
  vorbis_info = record
    version         : cint;
    channels        : cint;
    rate            : clong;

  { The below bitrate declarations are *hints*.
     Combinations of the three values carry the following implications:

     all three set to the same value:
       implies a fixed rate bitstream
     only nominal set:
       implies a VBR stream that averages the nominal bitrate.  No hard
       upper/lower limit
     upper and or lower set:
       implies a VBR bitstream that obeys the bitrate limits. nominal
       may also be set to give a nominal rate.
     none set:
       the coder does not care to speculate.
  }

    bitrate_upper   : clong;
    bitrate_nominal : clong;
    bitrate_lower   : clong;
    bitrate_window  : clong;
    codec_setup     : pointer;
  end;

{ vorbis_dsp_state buffers the current vorbis audio analysis/synthesis state.  The DSP state belongs to a specific logical bitstream }

  pvorbis_dsp_state = ^vorbis_dsp_state;
  vorbis_dsp_state = record
    analysisp       : cint;
    vi              : pvorbis_info;

    pcm             : ppcfloat;
    pcmret          : ppcfloat;
    pcm_storage     : cint;
    pcm_current     : cint;
    pcm_returned    : cint;

    preextrapolate  : cint;
    eofflag         : cint;

    lW              : clong;
    W               : clong;
    nW              : clong;
    centerW         : clong;

    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;

    glue_bits       : ogg_int64_t;
    time_bits       : ogg_int64_t;
    floor_bits      : ogg_int64_t;
    res_bits        : ogg_int64_t;

    backend_state   : pointer;
  end;

{ vorbis_block is a single block of data to be processed as part of
  the analysis/synthesis stream; it belongs to a specific logical
  bitstream, but is independant from other vorbis_blocks belonging to
  that logical bitstream. }

  palloc_chain = ^alloc_chain;
  alloc_chain = record
    ptr             : pointer;
    next            : palloc_chain;
  end;

  pvorbis_block = ^vorbis_block;
  vorbis_block = record
  { necessary stream state for linking to the framing abstraction }
    pcm             : ppcfloat;            { this is a pointer into local storage }
    opb             : oggpack_buffer;

    lW              : clong;
    W               : clong;
    nW              : clong;
    pcmend          : cint;
    mode            : cint;

    eofflag         : cint;
    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;
    vd              : pvorbis_dsp_state; { For read-only access of configuration }

  { local storage to avoid remallocing; it's up to the mapping to structure it }
    localstore      : pointer;
    localtop        : clong;
    localalloc      : clong;
    totaluse        : clong;
    reap            : palloc_chain;

  { bitmetrics for the frame }
    glue_bits       : clong;
    time_bits       : clong;
    floor_bits      : clong;
    res_bits        : clong;

    internal        : pointer;
  end;

{ vorbis_info contains all the setup information specific to the
  specific compression/decompression mode in progress (eg,
  psychoacoustic settings, channel setup, options, codebook
  etc). vorbis_info and substructures are in backends.h. }

{ the comments are not part of vorbis_info so that vorbis_info can be static storage }

  pvorbis_comment = ^vorbis_comment;
  vorbis_comment = record
  { unlimited user comment fields.  libvorbis writes 'libvorbis' whatever vendor is set to in encode }
    user_comments   : ^pcchar;
    comment_lengths : pcint;
    comments        : cint;
    vendor          : pcchar;
  end;

  ovectl_ratemanage_arg = record
    management_active : cint; { nonzero if bitrate management is active}
   {  hard lower limit (in kilobits per second) below which the stream bitrate
      will never be allowed for any given bitrate_hard_window seconds of time.}
    bitrate_hard_min : clong;
   {  hard upper limit (in kilobits per second) above which the stream bitrate
      will never be allowed for any given bitrate_hard_window seconds of time.}
    bitrate_hard_max : clong;
   {  the window period (in seconds) used to regulate the hard bitrate minimum
      and maximum}
    bitrate_hard_window : cdouble;
   {  soft lower limit (in kilobits per second) below which the average bitrate
      tracker will start nudging the bitrate higher.}
    bitrate_av_lo : clong;
   {  soft upper limit (in kilobits per second) above which the average bitrate
      tracker will start nudging the bitrate lower.}
    bitrate_av_hi : clong;
   {  the window period (in seconds) used to regulate the average bitrate
      minimum and maximum.}
    bitrate_av_window : cdouble;
   {  Regulates the relative centering of the average and hard windows; in
      libvorbis 1.0 and 1.0.1, the hard window regulation overlapped but
      followed the average window regulation. In libvorbis 1.1 a bit-reservoir
      interface replaces the old windowing interface; the older windowing
      interface is simulated and this field has no effect.}
    bitrate_av_window_center : cdouble;
  end;

  ovectl_ratemanage2_arg = record
    management_active : cint; { nonzero if bitrate management is active }
   {  Lower allowed bitrate limit in kilobits per second }
    bitrate_limit_min_kbps : clong;
   {  Upper allowed bitrate limit in kilobits per second }
    bitrate_limit_max_kbps : clong;
    bitrate_limit_reservoir_bits : clong; { Size of the bitrate reservoir in bits }
   { Regulates the bitrate reservoir's preferred fill level in a range from 0.0
   * to 1.0; 0.0 tries to bank bits to buffer against future bitrate spikes, 1.0
   * buffers against future sudden drops in instantaneous bitrate. Default is
   * 0.1 }
    bitrate_limit_reservoir_bias : cdouble;
   { Average bitrate setting in kilobits per second }
    bitrate_average_kbps : clong;
   { Slew rate limit setting for average bitrate adjustment; sets the minimum
   *  time in seconds the bitrate tracker may swing from one extreme to the
   *  other when boosting or damping average bitrate.}
    bitrate_average_damping : cdouble;
  end;

  pfiltercallback = procedure (pcm:ppcfloat;channels,samples:clong;filter_param:pointer);cdecl;

  pov_read_func = function  (ptr : pointer; size, nmemb : csize_t; datasource : pointer) : csize_t; cdecl;
  pov_seek_func = function  (datasource : pointer; offset : ogg_int64_t; whence : cint) : cint; cdecl;
  pov_close_func = function (datasource : pointer) : cint; cdecl;
  pov_tell_func = function  (datasource : pointer) : clong; cdecl;

  ov_callbacks = record
    read_func  : pov_read_func;
    seek_func  : pov_seek_func;
    close_func : pov_close_func;
    tell_func  : pov_tell_func;
  end;

  OggVorbis_File = record
    datasource        : pointer; { Pointer to a FILE *, etc. }
    seekable          : cint;
    offset            : ogg_int64_t;
    endp              : ogg_int64_t;
    oy                : ogg_sync_state;

    { If the FILE handle isn't seekable (eg, a pipe), only the current
       stream appears }
    links             : cint;
    offsets           : pogg_int64_t;
    dataoffsets       : pogg_int64_t;
    serialnos         : pclong;
    pcmlengths        : pogg_int64_t; { overloaded to maintain binary
                                        compatibility; x2 size, stores both
                                        beginning and end values }
    vi                : pvorbis_info;
    vc                : pvorbis_comment;

    { Decoding working state local storage }
    pcm_offset        : ogg_int64_t;
    ready_state       : cint;
    current_serialno  : clong;
    current_link      : cint;

    bittrack          : cdouble;
    samptrack         : cdouble;

    os                : ogg_stream_state; { take physical pages, weld into a logical
                                            stream of packets }
    vd                : vorbis_dsp_state; { central working state for the packet->PCM decoder }
    vb                : vorbis_block;     { local working space for packet->PCM decode }

    callbacks         : ov_callbacks;
  end;

  pOggVorbis_File = ^OggVorbis_File;

{ vorbislib ERRORS and return codes }
Const
  OV_FALSE          = cint(-1);
  OV_EOF            = cint(-2);
  OV_HOLE           = cint(-3);

  OV_EREAD          = cint(-128);
  OV_EFAULT         = cint(-129);
  OV_EIMPL          = cint(-130);
  OV_EINVAL         = cint(-131);
  OV_ENOTVORBIS     = cint(-132);
  OV_EBADHEADER     = cint(-133);
  OV_EVERSION       = cint(-134);
  OV_ENOTAUDIO      = cint(-135);
  OV_EBADPACKET     = cint(-136);
  OV_EBADLINK       = cint(-137);
  OV_ENOSEEK        = cint(-138);

  NOTOPEN   = cint(0);
  PARTOPEN  = cint(1);
  OPENED    = cint(2);
  STREAMSET = cint(3);
  INITSET   = cint(4);

  OV_ECTL_RATEMANAGE2_GET = cint($14);
  OV_ECTL_RATEMANAGE2_SET = cint($15);
  OV_ECTL_LOWPASS_GET     = cint($20);
  OV_ECTL_LOWPASS_SET     = cint($21);
  OV_ECTL_IBLOCK_GET      = cint($30);
  OV_ECTL_IBLOCK_SET      = cint($31);
  OV_ECTL_COUPLING_GET    = cint($40);
  OV_ECTL_COUPLING_SET    = cint($41);
  OV_ECTL_RATEMANAGE_GET  = cint($10);
  OV_ECTL_RATEMANAGE_SET  = cint($11);
  OV_ECTL_RATEMANAGE_AVG  = cint($12);
  OV_ECTL_RATEMANAGE_HARD = cint($13);


procedure vorbis_info_init(vi: pvorbis_info);
procedure vorbis_info_clear(vi: pvorbis_info);
function vorbis_info_blocksize(vi: pvorbis_info; zo: cint): cint;
procedure vorbis_comment_init(vc: pvorbis_comment);
procedure vorbis_comment_add(vc: pvorbis_comment; const comment: pcchar);
procedure vorbis_comment_add_tag(vc: pvorbis_comment; const tag: pcchar; const contents: pcchar);
function vorbis_comment_query(vc: pvorbis_comment; const tag: pcchar; count: cint): pcchar;
function vorbis_comment_query_count(vc: pvorbis_comment; const tag: pcchar): cint;
procedure vorbis_comment_clear(vc: pvorbis_comment);
function vorbis_block_init(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
function vorbis_block_clear(vb: pvorbis_block): cint;
procedure vorbis_dsp_clear(v: pvorbis_dsp_state);
function vorbis_granule_time(v: pvorbis_dsp_state; granulepos: ogg_int64_t): cdouble;
function vorbis_version_string(): pcchar;
function vorbis_analysis_init(v: pvorbis_dsp_state; vi: pvorbis_info): cint;
function vorbis_commentheader_out(vc: pvorbis_comment; op: pogg_packet): cint;
function vorbis_analysis_headerout(v: pvorbis_dsp_state; vc: pvorbis_comment; op: pogg_packet; op_comm: pogg_packet; op_code: pogg_packet): cint;
function vorbis_analysis_buffer(v: pvorbis_dsp_state; vals: cint): pcfloat;
function vorbis_analysis_wrote(v: pvorbis_dsp_state; vals: cint): cint;
function vorbis_analysis_blockout(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
function vorbis_analysis(vb: pvorbis_block; op: pogg_packet): cint;
function vorbis_bitrate_addblock(vb: pvorbis_block): cint;
function vorbis_bitrate_flushpacket(vd: pvorbis_dsp_state; op: pogg_packet): cint;
function vorbis_synthesis_idheader(op: pogg_packet): cint;
function vorbis_synthesis_headerin(vi: pvorbis_info; vc: pvorbis_comment; op: pogg_packet): cint;
function vorbis_synthesis_init(v: pvorbis_dsp_state; vi: pvorbis_info): cint;
function vorbis_synthesis_restart(v: pvorbis_dsp_state): cint;
function vorbis_synthesis(vb: pvorbis_block; op: pogg_packet): cint;
function vorbis_synthesis_trackonly(vb: pvorbis_block; op: pogg_packet): cint;
function vorbis_synthesis_blockin(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
function vorbis_synthesis_pcmout(v: pvorbis_dsp_state; pcm: pcfloat): cint;
function vorbis_synthesis_lapout(v: pvorbis_dsp_state; pcm: pcfloat): cint;
function vorbis_synthesis_read(v: pvorbis_dsp_state; samples: cint): cint;
function vorbis_packet_blocksize(vi: pvorbis_info; op: pogg_packet): clong;
function vorbis_synthesis_halfrate(v: pvorbis_info; flag: cint): cint;
function vorbis_synthesis_halfrate_p(v: pvorbis_info): cint;

function vorbis_encode_init(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint;
function vorbis_encode_setup_managed(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint;
function vorbis_encode_setup_vbr(vi: pvorbis_info; channels: clong; rate: clong; quality: cfloat): cint;
function vorbis_encode_init_vbr(vi: pvorbis_info; channels: clong; rate: clong; base_quality: cfloat): cint;
function vorbis_encode_setup_init(vi: pvorbis_info): cint;
function vorbis_encode_ctl(vi: pvorbis_info; number: cint; arg: pointer): cint;

function ov_clear(vf: pOggVorbis_File): cint;
function ov_fopen(const path: pcchar; vf: pOggVorbis_File): cint;
function ov_open(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint;
function ov_open_callbacks(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint;
function ov_test(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint;
function ov_test_callbacks(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint;
function ov_test_open(vf: pOggVorbis_File): cint;
function ov_bitrate(vf: pOggVorbis_File; i: cint): clong;
function ov_bitrate_instant(vf: pOggVorbis_File): clong;
function ov_streams(vf: pOggVorbis_File): clong;
function ov_seekable(vf: pOggVorbis_File): clong;
function ov_serialnumber(vf: pOggVorbis_File; i: cint): clong;
function ov_raw_total(vf: pOggVorbis_File; i: cint): ogg_int64_t;
function ov_pcm_total(vf: pOggVorbis_File; i: cint): ogg_int64_t;
function ov_time_total(vf: pOggVorbis_File; i: cint): cdouble;
function ov_raw_seek(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_pcm_seek(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_pcm_seek_page(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_time_seek(vf: pOggVorbis_File; pos: cdouble): cint;
function ov_time_seek_page(vf: pOggVorbis_File; pos: cdouble): cint;
function ov_raw_seek_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_pcm_seek_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_pcm_seek_page_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
function ov_time_seek_lap(vf: pOggVorbis_File; pos: cdouble): cint;
function ov_time_seek_page_lap(vf: pOggVorbis_File; pos: cdouble): cint;
function ov_raw_tell(vf: pOggVorbis_File): ogg_int64_t;
function ov_pcm_tell(vf: pOggVorbis_File): ogg_int64_t;
function ov_time_tell(vf: pOggVorbis_File): cdouble;
function ov_info(vf: pOggVorbis_File; link: cint): pvorbis_info;
function ov_comment(vf: pOggVorbis_File; link: cint): pvorbis_comment;
function ov_read_float(vf: pOggVorbis_File; pcm_channels: pppcfloat; samples: cint; bitstream: pcint): clong;
function ov_read_filter(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint; filter : pfiltercallback; filter_param: pointer): clong;
function ov_read(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint): clong;
function ov_crosslap(vf1: pOggVorbis_File; vf2: pOggVorbis_File): cint;
function ov_halfrate(vf: pOggVorbis_File; flag: cint): cint;
function ov_halfrate_p(vf: pOggVorbis_File): cint;

function IsVorbisloaded: boolean;
function InitVorbisInterface(const aLibs : Array of String): boolean; overload;
function DestroyVorbisInterface: boolean;

implementation

var
  Vorbisloaded: boolean = False;
  VorbisLib: Array of HModule;
resourcestring
  SFailedToLoadVorbis = 'Failed to load Vorbis library';

type
  p_vorbis_info_init = procedure(vi: pvorbis_info); cdecl;
  p_vorbis_info_clear = procedure(vi: pvorbis_info); cdecl;
  p_vorbis_info_blocksize = function(vi: pvorbis_info; zo: cint): cint; cdecl;
  p_vorbis_comment_init = procedure(vc: pvorbis_comment); cdecl;
  p_vorbis_comment_add = procedure(vc: pvorbis_comment; const comment: pcchar); cdecl;
  p_vorbis_comment_add_tag = procedure(vc: pvorbis_comment; const tag: pcchar; const contents: pcchar); cdecl;
  p_vorbis_comment_query = function(vc: pvorbis_comment; const tag: pcchar; count: cint): pcchar; cdecl;
  p_vorbis_comment_query_count = function(vc: pvorbis_comment; const tag: pcchar): cint; cdecl;
  p_vorbis_comment_clear = procedure(vc: pvorbis_comment); cdecl;
  p_vorbis_block_init = function(v: pvorbis_dsp_state; vb: pvorbis_block): cint; cdecl;
  p_vorbis_block_clear = function(vb: pvorbis_block): cint; cdecl;
  p_vorbis_dsp_clear = procedure(v: pvorbis_dsp_state); cdecl;
  p_vorbis_granule_time = function(v: pvorbis_dsp_state; granulepos: ogg_int64_t): cdouble; cdecl;
  p_vorbis_version_string = function(): pcchar; cdecl;
  p_vorbis_analysis_init = function(v: pvorbis_dsp_state; vi: pvorbis_info): cint; cdecl;
  p_vorbis_commentheader_out = function(vc: pvorbis_comment; op: pogg_packet): cint; cdecl;
  p_vorbis_analysis_headerout = function(v: pvorbis_dsp_state; vc: pvorbis_comment; op: pogg_packet; op_comm: pogg_packet; op_code: pogg_packet): cint; cdecl;
  p_vorbis_analysis_buffer = function(v: pvorbis_dsp_state; vals: cint): pcfloat; cdecl;
  p_vorbis_analysis_wrote = function(v: pvorbis_dsp_state; vals: cint): cint; cdecl;
  p_vorbis_analysis_blockout = function(v: pvorbis_dsp_state; vb: pvorbis_block): cint; cdecl;
  p_vorbis_analysis = function(vb: pvorbis_block; op: pogg_packet): cint; cdecl;
  p_vorbis_bitrate_addblock = function(vb: pvorbis_block): cint; cdecl;
  p_vorbis_bitrate_flushpacket = function(vd: pvorbis_dsp_state; op: pogg_packet): cint; cdecl;
  p_vorbis_synthesis_idheader = function(op: pogg_packet): cint; cdecl;
  p_vorbis_synthesis_headerin = function(vi: pvorbis_info; vc: pvorbis_comment; op: pogg_packet): cint; cdecl;
  p_vorbis_synthesis_init = function(v: pvorbis_dsp_state; vi: pvorbis_info): cint; cdecl;
  p_vorbis_synthesis_restart = function(v: pvorbis_dsp_state): cint; cdecl;
  p_vorbis_synthesis = function(vb: pvorbis_block; op: pogg_packet): cint; cdecl;
  p_vorbis_synthesis_trackonly = function(vb: pvorbis_block; op: pogg_packet): cint; cdecl;
  p_vorbis_synthesis_blockin = function(v: pvorbis_dsp_state; vb: pvorbis_block): cint; cdecl;
  p_vorbis_synthesis_pcmout = function(v: pvorbis_dsp_state; pcm: pcfloat): cint; cdecl;
  p_vorbis_synthesis_lapout = function(v: pvorbis_dsp_state; pcm: pcfloat): cint; cdecl;
  p_vorbis_synthesis_read = function(v: pvorbis_dsp_state; samples: cint): cint; cdecl;
  p_vorbis_packet_blocksize = function(vi: pvorbis_info; op: pogg_packet): clong; cdecl;
  p_vorbis_synthesis_halfrate = function(v: pvorbis_info; flag: cint): cint; cdecl;
  p_vorbis_synthesis_halfrate_p = function(v: pvorbis_info): cint; cdecl;

  p_vorbis_encode_init = function(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint; cdecl;
  p_vorbis_encode_setup_managed = function(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint; cdecl;
  p_vorbis_encode_setup_vbr = function(vi: pvorbis_info; channels: clong; rate: clong; quality: cfloat): cint; cdecl;
  p_vorbis_encode_init_vbr = function(vi: pvorbis_info; channels: clong; rate: clong; base_quality: cfloat): cint; cdecl;
  p_vorbis_encode_setup_init = function(vi: pvorbis_info): cint; cdecl;
  p_vorbis_encode_ctl = function(vi: pvorbis_info; number: cint; arg: pointer): cint; cdecl;
  p_ov_clear = function(vf: pOggVorbis_File): cint; cdecl;
  p_ov_fopen = function(const path: pcchar; vf: pOggVorbis_File): cint; cdecl;
  p_ov_open = function(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint; cdecl;
  p_ov_open_callbacks = function(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint; cdecl;
  p_ov_test = function(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint; cdecl;
  p_ov_test_callbacks = function(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint; cdecl;
  p_ov_test_open = function(vf: pOggVorbis_File): cint; cdecl;
  p_ov_bitrate = function(vf: pOggVorbis_File; i: cint): clong; cdecl;
  p_ov_bitrate_instant = function(vf: pOggVorbis_File): clong; cdecl;
  p_ov_streams = function(vf: pOggVorbis_File): clong; cdecl;
  p_ov_seekable = function(vf: pOggVorbis_File): clong; cdecl;
  p_ov_serialnumber = function(vf: pOggVorbis_File; i: cint): clong; cdecl;
  p_ov_raw_total = function(vf: pOggVorbis_File; i: cint): ogg_int64_t; cdecl;
  p_ov_pcm_total = function(vf: pOggVorbis_File; i: cint): ogg_int64_t; cdecl;
  p_ov_time_total = function(vf: pOggVorbis_File; i: cint): cdouble; cdecl;
  p_ov_raw_seek = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_pcm_seek = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_pcm_seek_page = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_time_seek = function(vf: pOggVorbis_File; pos: cdouble): cint; cdecl;
  p_ov_time_seek_page = function(vf: pOggVorbis_File; pos: cdouble): cint; cdecl;
  p_ov_raw_seek_lap = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_pcm_seek_lap = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_pcm_seek_page_lap = function(vf: pOggVorbis_File; pos: ogg_int64_t): cint; cdecl;
  p_ov_time_seek_lap = function(vf: pOggVorbis_File; pos: cdouble): cint; cdecl;
  p_ov_time_seek_page_lap = function(vf: pOggVorbis_File; pos: cdouble): cint; cdecl;
  p_ov_raw_tell = function(vf: pOggVorbis_File): ogg_int64_t; cdecl;
  p_ov_pcm_tell = function(vf: pOggVorbis_File): ogg_int64_t; cdecl;
  p_ov_time_tell = function(vf: pOggVorbis_File): cdouble; cdecl;
  p_ov_info = function(vf: pOggVorbis_File; link: cint): pvorbis_info; cdecl;
  p_ov_comment = function(vf: pOggVorbis_File; link: cint): pvorbis_comment; cdecl;
  p_ov_read_float = function(vf: pOggVorbis_File; pcm_channels: pppcfloat; samples: cint; bitstream: pcint): clong; cdecl;
  p_ov_read_filter = function(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint; filter : pfiltercallback; filter_param: pointer): clong; cdecl;
  p_ov_read = function(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint): clong; cdecl;
  p_ov_crosslap = function(vf1: pOggVorbis_File; vf2: pOggVorbis_File): cint; cdecl;
  p_ov_halfrate = function(vf: pOggVorbis_File; flag: cint): cint; cdecl;
  p_ov_halfrate_p = function(vf: pOggVorbis_File): cint; cdecl;

var
  _vorbis_info_init: p_vorbis_info_init = nil;
  _vorbis_info_clear: p_vorbis_info_clear = nil;
  _vorbis_info_blocksize: p_vorbis_info_blocksize = nil;
  _vorbis_comment_init: p_vorbis_comment_init = nil;
  _vorbis_comment_add: p_vorbis_comment_add = nil;
  _vorbis_comment_add_tag: p_vorbis_comment_add_tag = nil;
  _vorbis_comment_query: p_vorbis_comment_query = nil;
  _vorbis_comment_query_count: p_vorbis_comment_query_count = nil;
  _vorbis_comment_clear: p_vorbis_comment_clear = nil;
  _vorbis_block_init: p_vorbis_block_init = nil;
  _vorbis_block_clear: p_vorbis_block_clear = nil;
  _vorbis_dsp_clear: p_vorbis_dsp_clear = nil;
  _vorbis_granule_time: p_vorbis_granule_time = nil;
  _vorbis_version_string: p_vorbis_version_string = nil;
  _vorbis_analysis_init: p_vorbis_analysis_init = nil;
  _vorbis_commentheader_out: p_vorbis_commentheader_out = nil;
  _vorbis_analysis_headerout: p_vorbis_analysis_headerout = nil;
  _vorbis_analysis_buffer: p_vorbis_analysis_buffer = nil;
  _vorbis_analysis_wrote: p_vorbis_analysis_wrote = nil;
  _vorbis_analysis_blockout: p_vorbis_analysis_blockout = nil;
  _vorbis_analysis: p_vorbis_analysis = nil;
  _vorbis_bitrate_addblock: p_vorbis_bitrate_addblock = nil;
  _vorbis_bitrate_flushpacket: p_vorbis_bitrate_flushpacket = nil;
  _vorbis_synthesis_idheader: p_vorbis_synthesis_idheader = nil;
  _vorbis_synthesis_headerin: p_vorbis_synthesis_headerin = nil;
  _vorbis_synthesis_init: p_vorbis_synthesis_init = nil;
  _vorbis_synthesis_restart: p_vorbis_synthesis_restart = nil;
  _vorbis_synthesis: p_vorbis_synthesis = nil;
  _vorbis_synthesis_trackonly: p_vorbis_synthesis_trackonly = nil;
  _vorbis_synthesis_blockin: p_vorbis_synthesis_blockin = nil;
  _vorbis_synthesis_pcmout: p_vorbis_synthesis_pcmout = nil;
  _vorbis_synthesis_lapout: p_vorbis_synthesis_lapout = nil;
  _vorbis_synthesis_read: p_vorbis_synthesis_read = nil;
  _vorbis_packet_blocksize: p_vorbis_packet_blocksize = nil;
  _vorbis_synthesis_halfrate: p_vorbis_synthesis_halfrate = nil;
  _vorbis_synthesis_halfrate_p: p_vorbis_synthesis_halfrate_p = nil;

  _vorbis_encode_init: p_vorbis_encode_init = nil;
  _vorbis_encode_setup_managed: p_vorbis_encode_setup_managed = nil;
  _vorbis_encode_setup_vbr: p_vorbis_encode_setup_vbr = nil;
  _vorbis_encode_init_vbr: p_vorbis_encode_init_vbr = nil;
  _vorbis_encode_setup_init: p_vorbis_encode_setup_init = nil;
  _vorbis_encode_ctl: p_vorbis_encode_ctl = nil;
  _ov_clear: p_ov_clear = nil;
  _ov_fopen: p_ov_fopen = nil;
  _ov_open: p_ov_open = nil;
  _ov_open_callbacks: p_ov_open_callbacks = nil;
  _ov_test: p_ov_test = nil;
  _ov_test_callbacks: p_ov_test_callbacks = nil;
  _ov_test_open: p_ov_test_open = nil;
  _ov_bitrate: p_ov_bitrate = nil;
  _ov_bitrate_instant: p_ov_bitrate_instant = nil;
  _ov_streams: p_ov_streams = nil;
  _ov_seekable: p_ov_seekable = nil;
  _ov_serialnumber: p_ov_serialnumber = nil;
  _ov_raw_total: p_ov_raw_total = nil;
  _ov_pcm_total: p_ov_pcm_total = nil;
  _ov_time_total: p_ov_time_total = nil;
  _ov_raw_seek: p_ov_raw_seek = nil;
  _ov_pcm_seek: p_ov_pcm_seek = nil;
  _ov_pcm_seek_page: p_ov_pcm_seek_page = nil;
  _ov_time_seek: p_ov_time_seek = nil;
  _ov_time_seek_page: p_ov_time_seek_page = nil;
  _ov_raw_seek_lap: p_ov_raw_seek_lap = nil;
  _ov_pcm_seek_lap: p_ov_pcm_seek_lap = nil;
  _ov_pcm_seek_page_lap: p_ov_pcm_seek_page_lap = nil;
  _ov_time_seek_lap: p_ov_time_seek_lap = nil;
  _ov_time_seek_page_lap: p_ov_time_seek_page_lap = nil;
  _ov_raw_tell: p_ov_raw_tell = nil;
  _ov_pcm_tell: p_ov_pcm_tell = nil;
  _ov_time_tell: p_ov_time_tell = nil;
  _ov_info: p_ov_info = nil;
  _ov_comment: p_ov_comment = nil;
  _ov_read_float: p_ov_read_float = nil;
  _ov_read_filter: p_ov_read_filter = nil;
  _ov_read: p_ov_read = nil;
  _ov_crosslap: p_ov_crosslap = nil;
  _ov_halfrate: p_ov_halfrate = nil;
  _ov_halfrate_p: p_ov_halfrate_p = nil;

{$IFNDEF WINDOWS}
{ Try to load all library versions until you find or run out }
procedure LoadLibUnix(const aLibs : Array of String);
var i : integer;
begin
  for i := 0 to High(aLibs) do
  begin
    VorbisLib[i] := LoadLibrary(aLibs[i]);
  end;
end;

{$ELSE WINDOWS}
procedure LoadLibsWin(const aLibs : Array of String);
var i : integer;
begin
  for i := 0 to High(aLibs) do
  begin
    VorbisLib[i] := LoadLibrary(aLibs[i]);
  end;
end;

{$ENDIF WINDOWS}

function IsVorbisloaded: boolean;
begin
  Result := Vorbisloaded;
end;

procedure UnloadLibraries;
var i : integer;
begin
  Vorbisloaded := False;
  for i := 0 to High(VorbisLib) do
  if VorbisLib[i] <> NilHandle then
  begin
    FreeLibrary(VorbisLib[i]);
    VorbisLib[i] := NilHandle;
  end;
end;

function LoadLibraries(const aLibs : Array of String): boolean;
var i : integer;
begin
  SetLength(VorbisLib, Length(aLibs));
  Result := False;
  {$IFDEF WINDOWS}
  LoadLibsWin(aLibs);
  {$ELSE}
  LoadLibUnix(aLibs);
  {$ENDIF}
  for i := 0 to High(aLibs) do
  if VorbisLib[i] <> NilHandle then
     Result := true;
end;

function GetProcAddr(const module: Array of HModule; const ProcName: string): Pointer;
var i : integer;
begin
  for i := Low(module) to High(module) do
  if module[i] <> NilHandle then
  begin
    Result := GetProcAddress(module[i], PChar(ProcName));
    if Assigned(Result) then Exit;
  end;
end;

procedure LoadVorbisEntryPoints;
begin
  _vorbis_info_init := p_vorbis_info_init(GetProcAddr(VorbisLib, 'vorbis_info_init'));
  _vorbis_info_clear := p_vorbis_info_clear(GetProcAddr(VorbisLib, 'vorbis_info_clear'));
  _vorbis_info_blocksize := p_vorbis_info_blocksize(GetProcAddr(VorbisLib, 'vorbis_info_blocksize'));
  _vorbis_comment_init := p_vorbis_comment_init(GetProcAddr(VorbisLib, 'vorbis_comment_init'));
  _vorbis_comment_add := p_vorbis_comment_add(GetProcAddr(VorbisLib, 'vorbis_comment_add'));
  _vorbis_comment_add_tag := p_vorbis_comment_add_tag(GetProcAddr(VorbisLib, 'vorbis_comment_add_tag'));
  _vorbis_comment_query := p_vorbis_comment_query(GetProcAddr(VorbisLib, 'vorbis_comment_query'));
  _vorbis_comment_query_count := p_vorbis_comment_query_count(GetProcAddr(VorbisLib, 'vorbis_comment_query_count'));
  _vorbis_comment_clear := p_vorbis_comment_clear(GetProcAddr(VorbisLib, 'vorbis_comment_clear'));
  _vorbis_block_init := p_vorbis_block_init(GetProcAddr(VorbisLib, 'vorbis_block_init'));
  _vorbis_block_clear := p_vorbis_block_clear(GetProcAddr(VorbisLib, 'vorbis_block_clear'));
  _vorbis_dsp_clear := p_vorbis_dsp_clear(GetProcAddr(VorbisLib, 'vorbis_dsp_clear'));
  _vorbis_granule_time := p_vorbis_granule_time(GetProcAddr(VorbisLib, 'vorbis_granule_time'));
  _vorbis_version_string := p_vorbis_version_string(GetProcAddr(VorbisLib, 'vorbis_version_string'));
  _vorbis_analysis_init := p_vorbis_analysis_init(GetProcAddr(VorbisLib, 'vorbis_analysis_init'));
  _vorbis_commentheader_out := p_vorbis_commentheader_out(GetProcAddr(VorbisLib, 'vorbis_commentheader_out'));
  _vorbis_analysis_headerout := p_vorbis_analysis_headerout(GetProcAddr(VorbisLib, 'vorbis_analysis_headerout'));
  _vorbis_analysis_buffer := p_vorbis_analysis_buffer(GetProcAddr(VorbisLib, 'vorbis_analysis_buffer'));
  _vorbis_analysis_wrote := p_vorbis_analysis_wrote(GetProcAddr(VorbisLib, 'vorbis_analysis_wrote'));
  _vorbis_analysis_blockout := p_vorbis_analysis_blockout(GetProcAddr(VorbisLib, 'vorbis_analysis_blockout'));
  _vorbis_analysis := p_vorbis_analysis(GetProcAddr(VorbisLib, 'vorbis_analysis'));
  _vorbis_bitrate_addblock := p_vorbis_bitrate_addblock(GetProcAddr(VorbisLib, 'vorbis_bitrate_addblock'));
  _vorbis_bitrate_flushpacket := p_vorbis_bitrate_flushpacket(GetProcAddr(VorbisLib, 'vorbis_bitrate_flushpacket'));
  _vorbis_synthesis_idheader := p_vorbis_synthesis_idheader(GetProcAddr(VorbisLib, 'vorbis_synthesis_idheader'));
  _vorbis_synthesis_headerin := p_vorbis_synthesis_headerin(GetProcAddr(VorbisLib, 'vorbis_synthesis_headerin'));
  _vorbis_synthesis_init := p_vorbis_synthesis_init(GetProcAddr(VorbisLib, 'vorbis_synthesis_init'));
  _vorbis_synthesis_restart := p_vorbis_synthesis_restart(GetProcAddr(VorbisLib, 'vorbis_synthesis_restart'));
  _vorbis_synthesis := p_vorbis_synthesis(GetProcAddr(VorbisLib, 'vorbis_synthesis'));
  _vorbis_synthesis_trackonly := p_vorbis_synthesis_trackonly(GetProcAddr(VorbisLib, 'vorbis_synthesis_trackonly'));
  _vorbis_synthesis_blockin := p_vorbis_synthesis_blockin(GetProcAddr(VorbisLib, 'vorbis_synthesis_blockin'));
  _vorbis_synthesis_pcmout := p_vorbis_synthesis_pcmout(GetProcAddr(VorbisLib, 'vorbis_synthesis_pcmout'));
  _vorbis_synthesis_lapout := p_vorbis_synthesis_lapout(GetProcAddr(VorbisLib, 'vorbis_synthesis_lapout'));
  _vorbis_synthesis_read := p_vorbis_synthesis_read(GetProcAddr(VorbisLib, 'vorbis_synthesis_read'));
  _vorbis_packet_blocksize := p_vorbis_packet_blocksize(GetProcAddr(VorbisLib, 'vorbis_packet_blocksize'));
  _vorbis_synthesis_halfrate := p_vorbis_synthesis_halfrate(GetProcAddr(VorbisLib, 'vorbis_synthesis_halfrate'));
  _vorbis_synthesis_halfrate_p := p_vorbis_synthesis_halfrate_p(GetProcAddr(VorbisLib, 'vorbis_synthesis_halfrate_p'));

  _vorbis_encode_init := p_vorbis_encode_init(GetProcAddr(VorbisLib, 'vorbis_encode_init'));
  _vorbis_encode_setup_managed := p_vorbis_encode_setup_managed(GetProcAddr(VorbisLib, 'vorbis_encode_setup_managed'));
  _vorbis_encode_setup_vbr := p_vorbis_encode_setup_vbr(GetProcAddr(VorbisLib, 'vorbis_encode_setup_vbr'));
  _vorbis_encode_init_vbr := p_vorbis_encode_init_vbr(GetProcAddr(VorbisLib, 'vorbis_encode_init_vbr'));
  _vorbis_encode_setup_init := p_vorbis_encode_setup_init(GetProcAddr(VorbisLib, 'vorbis_encode_setup_init'));
  _vorbis_encode_ctl := p_vorbis_encode_ctl(GetProcAddr(VorbisLib, 'vorbis_encode_ctl'));
  _ov_clear := p_ov_clear(GetProcAddr(VorbisLib, 'ov_clear'));
  _ov_fopen := p_ov_fopen(GetProcAddr(VorbisLib, 'ov_fopen'));
  _ov_open := p_ov_open(GetProcAddr(VorbisLib, 'ov_open'));
  _ov_open_callbacks := p_ov_open_callbacks(GetProcAddr(VorbisLib, 'ov_open_callbacks'));
  _ov_test := p_ov_test(GetProcAddr(VorbisLib, 'ov_test'));
  _ov_test_callbacks := p_ov_test_callbacks(GetProcAddr(VorbisLib, 'ov_test_callbacks'));
  _ov_test_open := p_ov_test_open(GetProcAddr(VorbisLib, 'ov_test_open'));
  _ov_bitrate := p_ov_bitrate(GetProcAddr(VorbisLib, 'ov_bitrate'));
  _ov_bitrate_instant := p_ov_bitrate_instant(GetProcAddr(VorbisLib, 'ov_bitrate_instant'));
  _ov_streams := p_ov_streams(GetProcAddr(VorbisLib, 'ov_streams'));
  _ov_seekable := p_ov_seekable(GetProcAddr(VorbisLib, 'ov_seekable'));
  _ov_serialnumber := p_ov_serialnumber(GetProcAddr(VorbisLib, 'ov_serialnumber'));
  _ov_raw_total := p_ov_raw_total(GetProcAddr(VorbisLib, 'ov_raw_total'));
  _ov_pcm_total := p_ov_pcm_total(GetProcAddr(VorbisLib, 'ov_pcm_total'));
  _ov_time_total := p_ov_time_total(GetProcAddr(VorbisLib, 'ov_time_total'));
  _ov_raw_seek := p_ov_raw_seek(GetProcAddr(VorbisLib, 'ov_raw_seek'));
  _ov_pcm_seek := p_ov_pcm_seek(GetProcAddr(VorbisLib, 'ov_pcm_seek'));
  _ov_pcm_seek_page := p_ov_pcm_seek_page(GetProcAddr(VorbisLib, 'ov_pcm_seek_page'));
  _ov_time_seek := p_ov_time_seek(GetProcAddr(VorbisLib, 'ov_time_seek'));
  _ov_time_seek_page := p_ov_time_seek_page(GetProcAddr(VorbisLib, 'ov_time_seek_page'));
  _ov_raw_seek_lap := p_ov_raw_seek_lap(GetProcAddr(VorbisLib, 'ov_raw_seek_lap'));
  _ov_pcm_seek_lap := p_ov_pcm_seek_lap(GetProcAddr(VorbisLib, 'ov_pcm_seek_lap'));
  _ov_pcm_seek_page_lap := p_ov_pcm_seek_page_lap(GetProcAddr(VorbisLib, 'ov_pcm_seek_page_lap'));
  _ov_time_seek_lap := p_ov_time_seek_lap(GetProcAddr(VorbisLib, 'ov_time_seek_lap'));
  _ov_time_seek_page_lap := p_ov_time_seek_page_lap(GetProcAddr(VorbisLib, 'ov_time_seek_page_lap'));
  _ov_raw_tell := p_ov_raw_tell(GetProcAddr(VorbisLib, 'ov_raw_tell'));
  _ov_pcm_tell := p_ov_pcm_tell(GetProcAddr(VorbisLib, 'ov_pcm_tell'));
  _ov_time_tell := p_ov_time_tell(GetProcAddr(VorbisLib, 'ov_time_tell'));
  _ov_info := p_ov_info(GetProcAddr(VorbisLib, 'ov_info'));
  _ov_comment := p_ov_comment(GetProcAddr(VorbisLib, 'ov_comment'));
  _ov_read_float := p_ov_read_float(GetProcAddr(VorbisLib, 'ov_read_float'));
  _ov_read_filter := p_ov_read_filter(GetProcAddr(VorbisLib, 'ov_read_filter'));
  _ov_read := p_ov_read(GetProcAddr(VorbisLib, 'ov_read'));
  _ov_crosslap := p_ov_crosslap(GetProcAddr(VorbisLib, 'ov_crosslap'));
  _ov_halfrate := p_ov_halfrate(GetProcAddr(VorbisLib, 'ov_halfrate'));
  _ov_halfrate_p := p_ov_halfrate_p(GetProcAddr(VorbisLib, 'ov_halfrate_p'));
end;

procedure ClearVorbisEntryPoints;
begin
  _vorbis_info_init := nil;
  _vorbis_info_clear := nil;
  _vorbis_info_blocksize := nil;
  _vorbis_comment_init := nil;
  _vorbis_comment_add := nil;
  _vorbis_comment_add_tag := nil;
  _vorbis_comment_query := nil;
  _vorbis_comment_query_count := nil;
  _vorbis_comment_clear := nil;
  _vorbis_block_init := nil;
  _vorbis_block_clear := nil;
  _vorbis_dsp_clear := nil;
  _vorbis_granule_time := nil;
  _vorbis_version_string := nil;
  _vorbis_analysis_init := nil;
  _vorbis_commentheader_out := nil;
  _vorbis_analysis_headerout := nil;
  _vorbis_analysis_buffer := nil;
  _vorbis_analysis_wrote := nil;
  _vorbis_analysis_blockout := nil;
  _vorbis_analysis := nil;
  _vorbis_bitrate_addblock := nil;
  _vorbis_bitrate_flushpacket := nil;
  _vorbis_synthesis_idheader := nil;
  _vorbis_synthesis_headerin := nil;
  _vorbis_synthesis_init := nil;
  _vorbis_synthesis_restart := nil;
  _vorbis_synthesis := nil;
  _vorbis_synthesis_trackonly := nil;
  _vorbis_synthesis_blockin := nil;
  _vorbis_synthesis_pcmout := nil;
  _vorbis_synthesis_lapout := nil;
  _vorbis_synthesis_read := nil;
  _vorbis_packet_blocksize := nil;
  _vorbis_synthesis_halfrate := nil;
  _vorbis_synthesis_halfrate_p := nil;
  _vorbis_encode_init := nil;
  _vorbis_encode_setup_managed := nil;
  _vorbis_encode_setup_vbr := nil;
  _vorbis_encode_init_vbr := nil;
  _vorbis_encode_setup_init := nil;
  _vorbis_encode_ctl := nil;
  _ov_clear := nil;
  _ov_fopen := nil;
  _ov_open := nil;
  _ov_open_callbacks := nil;
  _ov_test := nil;
  _ov_test_callbacks := nil;
  _ov_test_open := nil;
  _ov_bitrate := nil;
  _ov_bitrate_instant := nil;
  _ov_streams := nil;
  _ov_seekable := nil;
  _ov_serialnumber := nil;
  _ov_raw_total := nil;
  _ov_pcm_total := nil;
  _ov_time_total := nil;
  _ov_raw_seek := nil;
  _ov_pcm_seek := nil;
  _ov_pcm_seek_page := nil;
  _ov_time_seek := nil;
  _ov_time_seek_page := nil;
  _ov_raw_seek_lap := nil;
  _ov_pcm_seek_lap := nil;
  _ov_pcm_seek_page_lap := nil;
  _ov_time_seek_lap := nil;
  _ov_time_seek_page_lap := nil;
  _ov_raw_tell := nil;
  _ov_pcm_tell := nil;
  _ov_time_tell := nil;
  _ov_info := nil;
  _ov_comment := nil;
  _ov_read_float := nil;
  _ov_read_filter := nil;
  _ov_read := nil;
  _ov_crosslap := nil;
  _ov_halfrate := nil;
  _ov_halfrate_p := nil;
end;

function InitVorbisInterface(const aLibs : array of String): boolean;
begin
  Result := IsVorbisloaded;
  if Result then
    exit;
  Result := LoadLibraries(aLibs);
  if not Result then
  begin
    UnloadLibraries;
    Exit;
  end;
  LoadVorbisEntryPoints;
  Vorbisloaded := True;
  Result := True;
end;

function DestroyVorbisInterface: boolean;
begin
  Result := not IsVorbisloaded;
  if Result then
    exit;
  ClearVorbisEntryPoints;
  UnloadLibraries;
  Result := True;
end;

procedure vorbis_info_init(vi: pvorbis_info);
begin
  if Assigned(_vorbis_info_init) then
    _vorbis_info_init(vi);
end;

procedure vorbis_info_clear(vi: pvorbis_info);
begin
  if Assigned(_vorbis_info_clear) then
    _vorbis_info_clear(vi);
end;

function vorbis_info_blocksize(vi: pvorbis_info; zo: cint): cint;
begin
  if Assigned(_vorbis_info_blocksize) then
    Result := _vorbis_info_blocksize(vi, zo)
  else
    Result := 0;
end;

procedure vorbis_comment_init(vc: pvorbis_comment);
begin
  if Assigned(_vorbis_comment_init) then
    _vorbis_comment_init(vc);
end;

procedure vorbis_comment_add(vc: pvorbis_comment; const comment: pcchar);
begin
  if Assigned(_vorbis_comment_add) then
    _vorbis_comment_add(vc, comment);
end;

procedure vorbis_comment_add_tag(vc: pvorbis_comment; const tag: pcchar; const contents: pcchar);
begin
  if Assigned(_vorbis_comment_add_tag) then
    _vorbis_comment_add_tag(vc, tag, contents);
end;

function vorbis_comment_query(vc: pvorbis_comment; const tag: pcchar; count: cint): pcchar;
begin
  if Assigned(_vorbis_comment_query) then
    Result := _vorbis_comment_query(vc, tag, count)
  else
    Result := nil;
end;

function vorbis_comment_query_count(vc: pvorbis_comment; const tag: pcchar): cint;
begin
  if Assigned(_vorbis_comment_query_count) then
    Result := _vorbis_comment_query_count(vc, tag)
  else
    Result := 0;
end;

procedure vorbis_comment_clear(vc: pvorbis_comment);
begin
  if Assigned(_vorbis_comment_clear) then
    _vorbis_comment_clear(vc);
end;

function vorbis_block_init(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
begin
  if Assigned(_vorbis_block_init) then
    Result := _vorbis_block_init(v, vb)
  else
    Result := 0;
end;

function vorbis_block_clear(vb: pvorbis_block): cint;
begin
  if Assigned(_vorbis_block_clear) then
    Result := _vorbis_block_clear(vb)
  else
    Result := 0;
end;

procedure vorbis_dsp_clear(v: pvorbis_dsp_state);
begin
  if Assigned(_vorbis_dsp_clear) then
    _vorbis_dsp_clear(v);
end;

function vorbis_granule_time(v: pvorbis_dsp_state; granulepos: ogg_int64_t): cdouble;
begin
  if Assigned(_vorbis_granule_time) then
    Result := _vorbis_granule_time(v, granulepos)
  else
    Result := 0.0;
end;

function vorbis_version_string(): pcchar;
begin
  if Assigned(_vorbis_version_string) then
    Result := _vorbis_version_string()
  else
    Result := nil;
end;

function vorbis_analysis_init(v: pvorbis_dsp_state; vi: pvorbis_info): cint;
begin
  if Assigned(_vorbis_analysis_init) then
    Result := _vorbis_analysis_init(v, vi)
  else
    Result := 0;
end;

function vorbis_commentheader_out(vc: pvorbis_comment; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_commentheader_out) then
    Result := _vorbis_commentheader_out(vc, op)
  else
    Result := 0;
end;

function vorbis_analysis_headerout(v: pvorbis_dsp_state; vc: pvorbis_comment; op: pogg_packet; op_comm: pogg_packet; op_code: pogg_packet): cint;
begin
  if Assigned(_vorbis_analysis_headerout) then
    Result := _vorbis_analysis_headerout(v, vc, op, op_comm, op_code)
  else
    Result := 0;
end;

function vorbis_analysis_buffer(v: pvorbis_dsp_state; vals: cint): pcfloat;
begin
  if Assigned(_vorbis_analysis_buffer) then
    Result := _vorbis_analysis_buffer(v, vals)
  else
    Result := nil;
end;

function vorbis_analysis_wrote(v: pvorbis_dsp_state; vals: cint): cint;
begin
  if Assigned(_vorbis_analysis_wrote) then
    Result := _vorbis_analysis_wrote(v, vals)
  else
    Result := 0;
end;

function vorbis_analysis_blockout(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
begin
  if Assigned(_vorbis_analysis_blockout) then
    Result := _vorbis_analysis_blockout(v, vb)
  else
    Result := 0;
end;

function vorbis_analysis(vb: pvorbis_block; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_analysis) then
    Result := _vorbis_analysis(vb, op)
  else
    Result := 0;
end;

function vorbis_bitrate_addblock(vb: pvorbis_block): cint;
begin
  if Assigned(_vorbis_bitrate_addblock) then
    Result := _vorbis_bitrate_addblock(vb)
  else
    Result := 0;
end;

function vorbis_bitrate_flushpacket(vd: pvorbis_dsp_state; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_bitrate_flushpacket) then
    Result := _vorbis_bitrate_flushpacket(vd, op)
  else
    Result := 0;
end;

function vorbis_synthesis_idheader(op: pogg_packet): cint;
begin
  if Assigned(_vorbis_synthesis_idheader) then
    Result := _vorbis_synthesis_idheader(op)
  else
    Result := 0;
end;

function vorbis_synthesis_headerin(vi: pvorbis_info; vc: pvorbis_comment; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_synthesis_headerin) then
    Result := _vorbis_synthesis_headerin(vi, vc, op)
  else
    Result := 0;
end;

function vorbis_synthesis_init(v: pvorbis_dsp_state; vi: pvorbis_info): cint;
begin
  if Assigned(_vorbis_synthesis_init) then
    Result := _vorbis_synthesis_init(v, vi)
  else
    Result := 0;
end;

function vorbis_synthesis_restart(v: pvorbis_dsp_state): cint;
begin
  if Assigned(_vorbis_synthesis_restart) then
    Result := _vorbis_synthesis_restart(v)
  else
    Result := 0;
end;

function vorbis_synthesis(vb: pvorbis_block; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_synthesis) then
    Result := _vorbis_synthesis(vb, op)
  else
    Result := 0;
end;

function vorbis_synthesis_trackonly(vb: pvorbis_block; op: pogg_packet): cint;
begin
  if Assigned(_vorbis_synthesis_trackonly) then
    Result := _vorbis_synthesis_trackonly(vb, op)
  else
    Result := 0;
end;

function vorbis_synthesis_blockin(v: pvorbis_dsp_state; vb: pvorbis_block): cint;
begin
  if Assigned(_vorbis_synthesis_blockin) then
    Result := _vorbis_synthesis_blockin(v, vb)
  else
    Result := 0;
end;

function vorbis_synthesis_pcmout(v: pvorbis_dsp_state; pcm: pcfloat): cint;
begin
  if Assigned(_vorbis_synthesis_pcmout) then
    Result := _vorbis_synthesis_pcmout(v, pcm)
  else
    Result := 0;
end;

function vorbis_synthesis_lapout(v: pvorbis_dsp_state; pcm: pcfloat): cint;
begin
  if Assigned(_vorbis_synthesis_lapout) then
    Result := _vorbis_synthesis_lapout(v, pcm)
  else
    Result := 0;
end;

function vorbis_synthesis_read(v: pvorbis_dsp_state; samples: cint): cint;
begin
  if Assigned(_vorbis_synthesis_read) then
    Result := _vorbis_synthesis_read(v, samples)
  else
    Result := 0;
end;

function vorbis_packet_blocksize(vi: pvorbis_info; op: pogg_packet): clong;
begin
  if Assigned(_vorbis_packet_blocksize) then
    Result := _vorbis_packet_blocksize(vi, op)
  else
    Result := 0;
end;

function vorbis_synthesis_halfrate(v: pvorbis_info; flag: cint): cint;
begin
  if Assigned(_vorbis_synthesis_halfrate) then
    Result := _vorbis_synthesis_halfrate(v, flag)
  else
    Result := 0;
end;

function vorbis_synthesis_halfrate_p(v: pvorbis_info): cint;
begin
  if Assigned(_vorbis_synthesis_halfrate_p) then
    Result := _vorbis_synthesis_halfrate_p(v)
  else
    Result := 0;
end;

function vorbis_encode_init(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint;
begin
  if Assigned(_vorbis_encode_init) then
    Result := _vorbis_encode_init(vi, channels, rate, max_bitrate, nominal_bitrate, min_bitrate)
  else
    Result := 0;
end;

function vorbis_encode_setup_managed(vi: pvorbis_info; channels: clong; rate: clong; max_bitrate: clong; nominal_bitrate: clong; min_bitrate: clong): cint;
begin
  if Assigned(_vorbis_encode_setup_managed) then
    Result := _vorbis_encode_setup_managed(vi, channels, rate, max_bitrate, nominal_bitrate, min_bitrate)
  else
    Result := 0;
end;

function vorbis_encode_setup_vbr(vi: pvorbis_info; channels: clong; rate: clong; quality: cfloat): cint;
begin
  if Assigned(_vorbis_encode_setup_vbr) then
    Result := _vorbis_encode_setup_vbr(vi, channels, rate, quality)
  else
    Result := 0;
end;

function vorbis_encode_init_vbr(vi: pvorbis_info; channels: clong; rate: clong; base_quality: cfloat): cint;
begin
  if Assigned(_vorbis_encode_init_vbr) then
    Result := _vorbis_encode_init_vbr(vi, channels, rate, base_quality)
  else
    Result := 0;
end;

function vorbis_encode_setup_init(vi: pvorbis_info): cint;
begin
  if Assigned(_vorbis_encode_setup_init) then
    Result := _vorbis_encode_setup_init(vi)
  else
    Result := 0;
end;

function vorbis_encode_ctl(vi: pvorbis_info; number: cint; arg: pointer): cint;
begin
  if Assigned(_vorbis_encode_ctl) then
    Result := _vorbis_encode_ctl(vi, number, arg)
  else
    Result := 0;
end;

function ov_clear(vf: pOggVorbis_File): cint;
begin
  if Assigned(_ov_clear) then
    Result := _ov_clear(vf)
  else
    Result := 0;
end;

function ov_fopen(const path: pcchar; vf: pOggVorbis_File): cint;
begin
  if Assigned(_ov_fopen) then
    Result := _ov_fopen(path, vf)
  else
    Result := 0;
end;

function ov_open(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint;
begin
  if Assigned(_ov_open) then
    Result := _ov_open(f, vf, initial, ibytes)
  else
    Result := 0;
end;

function ov_open_callbacks(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint;
begin
  if Assigned(_ov_open_callbacks) then
    Result := _ov_open_callbacks(datasource, vf, initial, ibytes, callbacks)
  else
    Result := 0;
end;

function ov_test(f: pFILE; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong): cint;
begin
  if Assigned(_ov_test) then
    Result := _ov_test(f, vf, initial, ibytes)
  else
    Result := 0;
end;

function ov_test_callbacks(datasource: pointer; vf: pOggVorbis_File; const initial: pcchar; ibytes: clong; callbacks: ov_callbacks): cint;
begin
  if Assigned(_ov_test_callbacks) then
    Result := _ov_test_callbacks(datasource, vf, initial, ibytes, callbacks)
  else
    Result := 0;
end;

function ov_test_open(vf: pOggVorbis_File): cint;
begin
  if Assigned(_ov_test_open) then
    Result := _ov_test_open(vf)
  else
    Result := 0;
end;

function ov_bitrate(vf: pOggVorbis_File; i: cint): clong;
begin
  if Assigned(_ov_bitrate) then
    Result := _ov_bitrate(vf, i)
  else
    Result := 0;
end;

function ov_bitrate_instant(vf: pOggVorbis_File): clong;
begin
  if Assigned(_ov_bitrate_instant) then
    Result := _ov_bitrate_instant(vf)
  else
    Result := 0;
end;

function ov_streams(vf: pOggVorbis_File): clong;
begin
  if Assigned(_ov_streams) then
    Result := _ov_streams(vf)
  else
    Result := 0;
end;

function ov_seekable(vf: pOggVorbis_File): clong;
begin
  if Assigned(_ov_seekable) then
    Result := _ov_seekable(vf)
  else
    Result := 0;
end;

function ov_serialnumber(vf: pOggVorbis_File; i: cint): clong;
begin
  if Assigned(_ov_serialnumber) then
    Result := _ov_serialnumber(vf, i)
  else
    Result := 0;
end;

function ov_raw_total(vf: pOggVorbis_File; i: cint): ogg_int64_t;
begin
  if Assigned(_ov_raw_total) then
    Result := _ov_raw_total(vf, i)
  else
    Result := 0;
end;

function ov_pcm_total(vf: pOggVorbis_File; i: cint): ogg_int64_t;
begin
  if Assigned(_ov_pcm_total) then
    Result := _ov_pcm_total(vf, i)
  else
    Result := 0;
end;

function ov_time_total(vf: pOggVorbis_File; i: cint): cdouble;
begin
  if Assigned(_ov_time_total) then
    Result := _ov_time_total(vf, i)
  else
    Result := 0.0;
end;

function ov_raw_seek(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_raw_seek) then
    Result := _ov_raw_seek(vf, pos)
  else
    Result := 0;
end;

function ov_pcm_seek(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_pcm_seek) then
    Result := _ov_pcm_seek(vf, pos)
  else
    Result := 0;
end;

function ov_pcm_seek_page(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_pcm_seek_page) then
    Result := _ov_pcm_seek_page(vf, pos)
  else
    Result := 0;
end;

function ov_time_seek(vf: pOggVorbis_File; pos: cdouble): cint;
begin
  if Assigned(_ov_time_seek) then
    Result := _ov_time_seek(vf, pos)
  else
    Result := 0;
end;

function ov_time_seek_page(vf: pOggVorbis_File; pos: cdouble): cint;
begin
  if Assigned(_ov_time_seek_page) then
    Result := _ov_time_seek_page(vf, pos)
  else
    Result := 0;
end;

function ov_raw_seek_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_raw_seek_lap) then
    Result := _ov_raw_seek_lap(vf, pos)
  else
    Result := 0;
end;

function ov_pcm_seek_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_pcm_seek_lap) then
    Result := _ov_pcm_seek_lap(vf, pos)
  else
    Result := 0;
end;

function ov_pcm_seek_page_lap(vf: pOggVorbis_File; pos: ogg_int64_t): cint;
begin
  if Assigned(_ov_pcm_seek_page_lap) then
    Result := _ov_pcm_seek_page_lap(vf, pos)
  else
    Result := 0;
end;

function ov_time_seek_lap(vf: pOggVorbis_File; pos: cdouble): cint;
begin
  if Assigned(_ov_time_seek_lap) then
    Result := _ov_time_seek_lap(vf, pos)
  else
    Result := 0;
end;

function ov_time_seek_page_lap(vf: pOggVorbis_File; pos: cdouble): cint;
begin
  if Assigned(_ov_time_seek_page_lap) then
    Result := _ov_time_seek_page_lap(vf, pos)
  else
    Result := 0;
end;

function ov_raw_tell(vf: pOggVorbis_File): ogg_int64_t;
begin
  if Assigned(_ov_raw_tell) then
    Result := _ov_raw_tell(vf)
  else
    Result := 0;
end;

function ov_pcm_tell(vf: pOggVorbis_File): ogg_int64_t;
begin
  if Assigned(_ov_pcm_tell) then
    Result := _ov_pcm_tell(vf)
  else
    Result := 0;
end;

function ov_time_tell(vf: pOggVorbis_File): cdouble;
begin
  if Assigned(_ov_time_tell) then
    Result := _ov_time_tell(vf)
  else
    Result := 0.0;
end;

function ov_info(vf: pOggVorbis_File; link: cint): pvorbis_info;
begin
  if Assigned(_ov_info) then
    Result := _ov_info(vf, link)
  else
    Result := nil;
end;

function ov_comment(vf: pOggVorbis_File; link: cint): pvorbis_comment;
begin
  if Assigned(_ov_comment) then
    Result := _ov_comment(vf, link)
  else
    Result := nil;
end;

function ov_read_float(vf: pOggVorbis_File; pcm_channels: pppcfloat; samples: cint; bitstream: pcint): clong;
begin
  if Assigned(_ov_read_float) then
    Result := _ov_read_float(vf, pcm_channels, samples, bitstream)
  else
    Result := 0;
end;

function ov_read_filter(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint; filter : pfiltercallback; filter_param: pointer): clong;
begin
  if Assigned(_ov_read_filter) then
    Result := _ov_read_filter(vf, buffer, length, bigendianp, word, sgned, bitstream, filter, filter_param)
  else
    Result := 0;
end;

function ov_read(vf: pOggVorbis_File; buffer: pcchar; length: cint; bigendianp: cint; word: cint; sgned: cint; bitstream: pcint): clong;
begin
  if Assigned(_ov_read) then
    Result := _ov_read(vf, buffer, length, bigendianp, word, sgned, bitstream)
  else
    Result := 0;
end;

function ov_crosslap(vf1: pOggVorbis_File; vf2: pOggVorbis_File): cint;
begin
  if Assigned(_ov_crosslap) then
    Result := _ov_crosslap(vf1, vf2)
  else
    Result := 0;
end;

function ov_halfrate(vf: pOggVorbis_File; flag: cint): cint;
begin
  if Assigned(_ov_halfrate) then
    Result := _ov_halfrate(vf, flag)
  else
    Result := 0;
end;

function ov_halfrate_p(vf: pOggVorbis_File): cint;
begin
  if Assigned(_ov_halfrate_p) then
    Result := _ov_halfrate_p(vf)
  else
    Result := 0;
end;

end.

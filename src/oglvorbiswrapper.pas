{
 OGLVorbisWrapper:
   Wrapper for Vorbis library

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLVorbisWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libVorbis_dyn, OGLOGGWrapper, ctypes;

type

  { IVorbisInfo }

  IVorbisInfo = interface(IUnknown)
  ['{BDCC1BB5-4910-478E-BC00-AE597AD1B869}']

  function Ref : pvorbis_info;

  procedure Init;
  procedure Done;

  function GetBitrateLower : Int64;
  function GetBitrateNominal : Int64;
  function GetBitrateUpper : Int64;
  function GetBitrateWindow : Int64;
  function GetChannels : Integer;
  function GetCodecSetup : pointer;
  function GetRate : Int64;
  function GetVersion : Integer;

  procedure SetBitrateLower(AValue : Int64);
  procedure SetBitrateNominal(AValue : Int64);
  procedure SetBitrateUpper(AValue : Int64);
  procedure SetBitrateWindow(AValue : Int64);
  procedure SetChannels(AValue : Integer);
  procedure SetCodecSetup(AValue : pointer);
  procedure SetRate(AValue : Int64);
  procedure SetVersion(AValue : Integer);

  property Version : Integer read GetVersion write SetVersion;
  property Channels : Integer read GetChannels write SetChannels;
  property Rate : Int64 read GetRate write SetRate;
  property BitrateUpper   : Int64 read GetBitrateUpper write SetBitrateUpper;
  property BitrateNominal : Int64 read GetBitrateNominal write SetBitrateNominal;
  property BitrateLower   : Int64 read GetBitrateLower write SetBitrateLower;
  property BitrateWindow  : Int64 read GetBitrateWindow write SetBitrateWindow;

  property CodecSetup : pointer read GetCodecSetup write SetCodecSetup;
  end;

  { IVorbisBlock }

  IVorbisBlock = interface(IUnknown)
  ['{3D3EAEBF-F44E-494E-B6A2-4082E6EEAA24}']
  function Ref : pvorbis_block;

  procedure Done;

  procedure BitrateAdd;
  function Analysis(OgPac : IOGGPacket) : integer;
  function Synthesis(OgPac: IOGGPacket) : integer;
  function TrackOnly(OgPac: IOGGPacket) : Integer;
  end;

  { IVorbisState }

  IVorbisState = interface(IUnknown)
  ['{65917275-0282-45B7-B66B-423CEDF4A390}']
  function Ref : pvorbis_dsp_state;

  function Init : Boolean;
  procedure Done;

  function InitBlock : IVorbisBlock;

  function Info : IVorbisInfo;
  end;

  { IVorbisEncoder }

  IVorbisEncoder = interface(IVorbisState)
  ['{FEBA3D29-FB43-41B7-925F-83AFF694C9CF}']

  function InitABR(channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : Boolean;
  function InitVBR(channels: Integer; rate: Int64; base_quality: Single): Boolean;

  function SetupManaged(channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64): Boolean;
  function SetupVBR(channels: Integer; rate: Int64; quality: Single): Boolean;
  function SetupInit: Boolean;

  procedure HeaderOut(vc : IOGGComment; header, header_comm, header_code : IOGGPacket);

  function  GetBuffer(BufSize : Integer) : Pointer;
  procedure Wrote(Sz : Integer);

  function BlockOut(b : IVorbisBlock) : Integer;
  function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

  function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { IVorbisDecoder }

  IVorbisDecoder = interface(IVorbisState)
  ['{6BAAAF6B-4353-424F-A89A-EA0C283326E0}']

  function IDHeader(op: IOGGPacket): integer;
  function HeaderIn(vc: IOGGComment; op: IOGGPacket): integer;
  function Restart : Integer;

  function BlockIn(b : IVorbisBlock) : Integer;

  function PCMOut(pcm: pointer): Integer;
  function LapOut(pcm: pointer): Integer;
  function Read(samples: Integer): Integer;
  function BlockSize(op: IOGGPacket): Longint;
  function Halfrate(flag : Boolean) : Integer;
  function IsHalfrate : Boolean;
  end;

  { TVorbisState }

  TVorbisState = class(TInterfacedObject)
  private
    FRef : vorbis_dsp_state;
    FInfo : IVorbisInfo;
    procedure Done;
  public
    constructor Create(inf : IVorbisInfo);
    destructor Destroy; override;

    function InitBlock : IVorbisBlock;

    function Ref : pvorbis_dsp_state; inline;
    function Info : IVorbisInfo; inline;
  end;

  { TVorbisEncoder }

  TVorbisEncoder = class(TVorbisState, IVorbisEncoder)
  private
    function Init : Boolean;
  public
    constructor Create(inf : IVorbisInfo); overload;
    constructor Create(inf : IVorbisInfo;
      channels: Integer; rate: Int64; base_quality: Single); overload;
    constructor Create(inf : IVorbisInfo; channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64); overload;

    function InitABR(channels: Integer;
              rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : Boolean;
    function InitVBR(channels: Integer; rate: Int64; base_quality: Single): Boolean;

    function SetupManaged(channels: Integer;
              rate, max_bitrate, nominal_bitrate, min_bitrate: Int64): Boolean;
    function SetupVBR(channels: Integer; rate: Int64; quality: Single): Boolean;
    function SetupInit: Boolean;

    procedure HeaderOut(vc : IOGGComment; header, header_comm, header_code : IOGGPacket);

    function  GetBuffer(BufSize : Integer) : Pointer;
    procedure Wrote(Sz : Integer);

    function BlockOut(b : IVorbisBlock) : Integer;
    function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

    function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { TVorbisDecoder }

  TVorbisDecoder = class(TVorbisState, IVorbisDecoder)
  private
    function Init : Boolean;
  public
    constructor Create(inf : IVorbisInfo); overload;
    constructor CreateNotInit(inf : IVorbisInfo);

    function IDHeader(op: IOGGPacket): Integer;
    function HeaderIn(vc: IOGGComment; op: IOGGPacket): Integer;
    function Restart : Integer;

    function BlockIn(b : IVorbisBlock) : Integer;

    function PCMOut(pcm: pointer): Integer;
    function LapOut(pcm: pointer): Integer;
    function Read(samples: Integer): Integer;
    function BlockSize(op: IOGGPacket): Longint;
    function Halfrate(flag : Boolean) : Integer;
    function IsHalfrate : Boolean;
  end;

  { TRefVorbisComment }

  TRefVorbisComment = class(TInterfacedObject, IOGGComment)
  private
    FRef : pvorbis_comment;
    procedure Init;
    procedure Done;
  public
    function Ref : Pointer; inline;

    constructor Create(aRef : pvorbis_comment);

    procedure Add(const comment: String);
    procedure AddTag(const tag, value: String);
    function Query(const tag: String; index: Integer): String;
    function QueryCount(const tag: String): Integer;
  end;

  { TUniqVorbisComment }

  TUniqVorbisComment = class(TRefVorbisComment)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TRefVorbisInfo }

  TRefVorbisInfo = class(TInterfacedObject, IVorbisInfo)
  private
    FPRef : pvorbis_info;
    procedure Init;
    procedure Done;
  public
    function Ref : pvorbis_info; inline;

    constructor Create(aRef : pvorbis_info);

    function GetBitrateLower : Int64;
    function GetBitrateNominal : Int64;
    function GetBitrateUpper : Int64;
    function GetBitrateWindow : Int64;
    function GetChannels : Integer;
    function GetCodecSetup : pointer;
    function GetRate : Int64;
    function GetVersion : Integer;

    procedure SetBitrateLower(AValue : Int64);
    procedure SetBitrateNominal(AValue : Int64);
    procedure SetBitrateUpper(AValue : Int64);
    procedure SetBitrateWindow(AValue : Int64);
    procedure SetChannels(AValue : Integer);
    procedure SetCodecSetup(AValue : pointer);
    procedure SetRate(AValue : Int64);
    procedure SetVersion(AValue : Integer);
  end;

  { TUniqVorbisInfo }

  TUniqVorbisInfo = class(TRefVorbisInfo)
  private
    FRef : vorbis_info;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TVorbisBlock }

  TVorbisBlock = class(TInterfacedObject, IVorbisBlock)
  private
    FRef : vorbis_block;
    procedure Done;
  public
    function Ref : pvorbis_block; inline;

    constructor Create;
    destructor Destroy; override;

    procedure BitrateAdd;
    function  Analysis(OgPac : IOGGPacket) : Integer;
    function  Synthesis(OgPac: IOGGPacket) : Integer;
    function  TrackOnly(OgPac: IOGGPacket) : Integer;
  end;

  pOggVorbisEnc = ^OggVorbisEnc;
  OggVorbisEnc = record
    enc   : IVorbisEncoder;
    block : IVorbisBlock;
    info  : IVorbisInfo;
    oggs  : IOGGStreamState;
    bitdepth : Integer;
    quality : Single;
    mode : TOGGSoundEncoderMode;
  end;

  { TVorbisOggEncoder }

  TVorbisOggEncoder = class(TOGGSoundEncoder)
  private
    fRef : pOggVorbisEnc;
    fComm : IOGGComment;
  protected
    procedure Init(aMode : TOGGSoundEncoderMode;
                   aChannels : Cardinal;
                   aFreq, aBitrate, aBitdepth : Cardinal;
                   aQuality : Single;
                   aComments : IOGGComment); override;
    procedure Done; override;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetMode : TOGGSoundEncoderMode; override;
    function GetQuality : Single; override;
    function GetVersion : Integer; override;

    function OggStream : IOGGStreamState; inline;
    function Encoder : IVorbisEncoder; inline;
    procedure FlushOggStream; virtual;
  public
    function Ref : pOggVorbisEnc; inline;

    constructor Create(aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq,
      aBitrate, aBitdepth : Cardinal; aQuality : Single; aComments : IOGGComment);
    destructor Destroy; override;

    function  Comments : IOGGComment; override;

    function  DataMode : TOGGSoundDataMode; override;

    function  WriteData(Buffer : Pointer; Count : Integer;
                               {%H-}Par : Pointer) : Integer; override;
    procedure WriteHeader({%H-}Par : Pointer); override;
    procedure Close({%H-}Par : Pointer); override;
    procedure Flush({%H-}Par : Pointer); override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggStreamingEncoder }

  TVorbisOggStreamingEncoder = class(TVorbisOggEncoder)
  private
    fStream : TStream;
  protected
    procedure FlushOggStream; override;
  public
    constructor Create(aStream : TStream; aMode : TOGGSoundEncoderMode;
      aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
      aQuality : Single; aComments : IOGGComment);
    procedure SetStream(aStream : TStream);
    procedure ReInitEncoder;
  end;

  pOggVorbisFileDec = ^OggVorbisFileDec;
  OggVorbisFileDec = record
    ovfile    : OggVorbis_File;
    info      : IVorbisInfo;
    callbacks : ov_callbacks;
  end;

  { TVorbisOggDecoder }

  TVorbisOggDecoder = class(TOGGSoundDecoder)
  private
    fRef  : Pointer;
    fComm : IOGGComment;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetVersion : Integer; override;

    function VorbisInfo : IVorbisInfo; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function  Comments : IOGGComment; override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggFileDecoder }

  TVorbisOggFileDecoder = class(TVorbisOggDecoder)
  protected
    procedure Init; override;
    procedure Done; override;

    function VorbisInfo : IVorbisInfo; override;
    function OVFile : pOggVorbis_File; inline;
  public
    function Ref : pOggVorbisFileDec; inline;

    function  DataMode : TOGGSoundDataMode; override;

    function  ReadData(Buffer : Pointer; Sz : Integer;
                              {%H-}Par : Pointer) : Integer; override;
    procedure ResetToStart; override;
  end;

  { not needed vorbisfile library }

  pOggVorbisDec = ^OggVorbisDec;
  OggVorbisDec = record
    dec   : IVorbisDecoder;
    block : IVorbisBlock;
    osyn  : IOGGSyncState;
    ostr  : IOGGStreamState;
    page  : IOGGPage;
    pack  : IOGGPacket;
    pcm_buffer : Pointer;
    state : Byte;
  end;

  { TVorbisOggAltDecoder }

  TVorbisOggAltDecoder = class(TVorbisOggDecoder)
  private
    function ReadDataToOgg : Integer;
    procedure ReadHeaders;
  protected
    procedure Init; override;
    procedure Done; override;

    function VorbisInfo : IVorbisInfo; override;

    function OggState : IOGGSyncState; inline;
    function OggStream : IOGGStreamState; inline;
    function OggPage : IOGGPage; inline;
    function OggPacket : IOGGPacket; inline;
    function Decoder : IVorbisDecoder; inline;

    class function DecoderReadBufferSize : Integer; virtual;
  public
    function Ref : pOggVorbisDec; inline;

    function  DataMode : TOGGSoundDataMode; override;

    function  ReadData(Buffer : Pointer; Sz : Integer;
                              {%H-}Par : Pointer) : Integer; override;
    procedure ResetToStart; override;
  end;

  { TVorbisOggStreamingFileDecoder }

  TVorbisOggStreamingFileDecoder = class(TVorbisOggFileDecoder)
  private
    fStream : TStream;
  protected
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; override;
    function DoSeek(_offset:Int64; _whence:Integer): Integer; override;
    function DoTell:Int64; override;
  public
    constructor Create(aStream : TStream);
    procedure SetStream(aStream : TStream);
  end;

  { TVorbisOggStreamingAltDecoder }

  TVorbisOggStreamingAltDecoder = class(TVorbisOggAltDecoder)
  private
    fStream : TStream;
  protected
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; override;
  public
    constructor Create(aStream : TStream);
    procedure SetStream(aStream : TStream);
  end;

  { TVorbisFile }

  TVorbisFile = class(TOGGSoundFile)
  protected
    function InitEncoder(aMode : TOGGSoundEncoderMode;
                   aChannels : Cardinal;
                   aFreq, aBitrate, aBitdepth : Cardinal;
                   aQuality : Single;
                   aComments : IOGGComment) : TOGGSoundEncoder; override;
    function InitDecoder : TOGGSoundDecoder; override;
  end;

  { TVorbisFileAlt }

  TVorbisFileAlt = class(TVorbisFile)
  protected
    function InitDecoder : TOGGSoundDecoder; override;
  end;

  { TVorbis }

  TVorbis = class
  public
    class function NewComment : IOGGComment;
    class function RefComment(aRef : pvorbis_comment) : IOGGComment;
    class function NewEncoder(inf : IVorbisInfo) : IVorbisEncoder;
    class function NewEncoderVBR(inf : IVorbisInfo;
      channels: Integer; rate: Int64; base_quality: Single) : IVorbisEncoder;
    class function NewEncoderABR(inf : IVorbisInfo; channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : IVorbisEncoder;
    class function NewInfo : IVorbisInfo;
    class function RefInfo(aRef : pvorbis_info) : IVorbisInfo;
    class function NewDecoder(inf : IVorbisInfo) : IVorbisDecoder;

    class function NewOggStreamEncoder(aStream : TStream;
                                       aMode : TOGGSoundEncoderMode;
                                       aChannels : Cardinal;
                                       aFreq, aBitrate, aBitdepth : Cardinal;
                                       aQuality : Single;
                                       aComments : IOGGComment) : TVorbisOggEncoder;
    class function NewOggStreamFileDecoder(aStream : TStream) : TVorbisOggDecoder;
    class function NewOggStreamAltDecoder(aStream : TStream) : TVorbisOggDecoder;

    class procedure UninterleaveSamples(BufW, Buffer : Pointer;
            samples, Channels, BitDepth : integer);

    class function VorbisLibsLoad(const aVorbisLibs : Array of String) : Boolean;
    class function VorbisLibsLoadDefault : Boolean;
    class function IsVorbisLibsLoaded : Boolean;
    class function VorbisLibsUnLoad : Boolean;
  end;

  EVorbis = class(Exception);

implementation

const
  sCantInitEncoder = 'Cant initialize encoder';
  sCantInitAnalysis = 'Cant initialize analysis';
  sCantInitSynthesis = 'Cant initialize synthesis';
  sWrongOggStream = 'Input does not appear to be an Ogg bitstream';
  sWrongHeader = 'Error reading initial header packet';
  sNotVorbis = 'This Ogg bitstream does not contain Vorbis audio data';
  sCorruptSecondaryHeader = 'Corrupt secondary header';
  sUnexpectedEndOfFile = 'End of file before finding all Vorbis headers';
  sVorbisError = 'Vorbis error %d';

function ops_read_func(ptr : pointer; size, nmemb: csize_t; datasource : Pointer): csize_t; cdecl;
begin
  if (size = 0) or (nmemb = 0) then begin Result := 0; Exit; end;
  try
    Result := Int64(TVorbisOggDecoder(datasource).DoRead(ptr, size * nmemb)) div Int64(size);
  except
    Result := 0;
  end;
end;

function ops_seek_func(datasource : pointer; offset: Int64; whence: Integer): Integer; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DoSeek(offset, whence);
end;

function ops_close_func(datasource : pointer): Integer; cdecl;
begin
  result := 0; 
end;

function ops_tell_func(datasource : pointer): clong; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DoTell;
end;

{ TVorbisOggDecoder }

constructor TVorbisOggDecoder.Create;
begin
  Init;
end;

destructor TVorbisOggDecoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggDecoder.Comments : IOGGComment;
begin
  Result := fComm;
end;

function TVorbisOggDecoder.Ready : Boolean;
begin
  Result := Assigned(fRef);
end;

function TVorbisOggDecoder.GetBitdepth : Cardinal;
begin
  Result := 16;
end;

function TVorbisOggDecoder.GetBitrate : Cardinal;
begin
  Result := VorbisInfo.BitrateNominal;
end;

function TVorbisOggDecoder.GetChannels : Cardinal;
begin
  Result := VorbisInfo.Channels;
end;

function TVorbisOggDecoder.GetFrequency : Cardinal;
begin
  Result := VorbisInfo.Rate;
end;

function TVorbisOggDecoder.GetVersion : Integer;
begin
  Result := VorbisInfo.Version;
end;

{ TVorbisDecoder }

function TVorbisDecoder.Init : Boolean;
begin
  Result := vorbis_synthesis_init(@FRef, Info.Ref) = 0;
end;

constructor TVorbisDecoder.Create(inf : IVorbisInfo);
begin
  inherited Create(inf);
  if not Init then
    raise EVorbis.Create(sCantInitSynthesis);
end;

constructor TVorbisDecoder.CreateNotInit(inf : IVorbisInfo);
begin
  inherited Create(inf);
end;

function TVorbisDecoder.IDHeader(op : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_idheader(op.Ref);
end;

function TVorbisDecoder.HeaderIn(vc : IOGGComment; op : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_headerin(Info.Ref, vc.Ref, op.Ref);
end;

function TVorbisDecoder.Restart : Integer;
begin
  Result := vorbis_synthesis_restart(Ref);
end;

function TVorbisDecoder.BlockIn(b : IVorbisBlock) : Integer;
begin
  Result := vorbis_synthesis_blockin(Ref, b.Ref);
end;

function TVorbisDecoder.PCMOut(pcm : pointer) : Integer;
begin
  Result := vorbis_synthesis_pcmout(Ref, pcm);
end;

function TVorbisDecoder.LapOut(pcm : pointer) : Integer;
begin
  Result := vorbis_synthesis_lapout(Ref, pcm);
end;

function TVorbisDecoder.Read(samples : Integer) : Integer;
begin
  Result := vorbis_synthesis_read(Ref, samples);
end;

function TVorbisDecoder.BlockSize(op : IOGGPacket) : Longint;
begin
  Result := vorbis_packet_blocksize(Info.Ref, op.Ref);
end;

function TVorbisDecoder.Halfrate(flag : Boolean) : Integer;
begin
  Result := vorbis_synthesis_halfrate(Info.Ref, integer(flag));
end;

function TVorbisDecoder.IsHalfrate : Boolean;
begin
  Result := Boolean(vorbis_synthesis_halfrate_p(Info.Ref));
end;

{ TUniqVorbisComment }

constructor TUniqVorbisComment.Create;
begin
  Init;
end;

destructor TUniqVorbisComment.Destroy;
begin
  Done;
  inherited Destroy;
end;

{ TVorbisFile }

function TVorbisFile.InitEncoder(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
  aQuality : Single; aComments : IOGGComment) : TOGGSoundEncoder;
begin
  Result := TVorbis.NewOggStreamEncoder(Stream, aMode, aChannels, aFreq,
                                            aBitrate,
                                            aBitdepth, aQuality,
                                            aComments);
end;

function TVorbisFile.InitDecoder : TOGGSoundDecoder;
begin
  Result := TVorbis.NewOggStreamFileDecoder(Stream);
end;

{ TVorbisFileAlt }

function TVorbisFileAlt.InitDecoder : TOGGSoundDecoder;
begin
  Result := TVorbis.NewOggStreamAltDecoder(Stream);
end;

{ TVorbisOggFileDecoder }

procedure TVorbisOggFileDecoder.Init;
var cError : Integer;
begin
  fRef := GetMem(Sizeof(OggVorbisFileDec));
  FillByte(fRef^, Sizeof(OggVorbisFileDec), 0);

  Ref^.callbacks.read_func := @ops_read_func;
  Ref^.callbacks.seek_func := @ops_seek_func;
  Ref^.callbacks.close_func := @ops_close_func;
  Ref^.callbacks.tell_func := @ops_tell_func;

  cError := ov_open_callbacks(Pointer(Self), OVFile, nil, 0, Ref^.callbacks);
  if cError <> 0 then
    raise EVorbis.CreateFmt(sVorbisError, [cError]);

  fComm := TVorbis.RefComment(ov_comment(OVFile, -1));

  Ref^.info := TVorbis.RefInfo(ov_info(OVFile, -1));
end;

procedure TVorbisOggFileDecoder.Done;
begin
  fComm := nil;
  if Assigned(fRef) then
  begin
    Ref^.info := nil;
    if (OVFile^.ready_state <> NOTOPEN) then
      ov_clear(OVFile);
    FreeMemAndNil(fRef);
  end;
end;

function TVorbisOggFileDecoder.OVFile : pOggVorbis_File;
begin
  Result := @(Ref^.ovfile);
end;

function TVorbisOggFileDecoder.VorbisInfo : IVorbisInfo;
begin
  Result := Ref^.info;
end;

function TVorbisOggFileDecoder.Ref : pOggVorbisFileDec;
begin
  Result := pOggVorbisFileDec(fRef);
end;

function TVorbisOggFileDecoder.DataMode : TOGGSoundDataMode;
begin
  Result := odmBytes;
end;

function TVorbisOggFileDecoder.ReadData(Buffer : Pointer; Sz : Integer;
  Par : Pointer) : Integer;
var
  Size, Section, Res: Integer;
begin
  Size := 0;
  while (Size < Sz) do begin
    Res := ov_read(fRef, @(PByte(Buffer)[Size]), Sz - Size, 0, 2, 1, @Section);
    if Res > 0 then inc(Size, Res) else break;
  end;
  Result := Size;
end;

procedure TVorbisOggFileDecoder.ResetToStart;
begin
  ov_pcm_seek(fRef, 0);
end;

{ TVorbisOggAltDecoder }

function TVorbisOggAltDecoder.ReadDataToOgg : Integer;
var aBuffer : Pointer;
begin
  aBuffer := OggState.Buffer(DecoderReadBufferSize);
  if Assigned(aBuffer) then
  begin
    Result := DoRead(aBuffer, DecoderReadBufferSize);
    OggState.Wrote(Result);
  end else
    Result := -1;
end;

procedure TVorbisOggAltDecoder.ReadHeaders;
var i, len, res : Integer;
begin
  len := ReadDataToOgg;
  if len > 0 then
  begin
    Ref^.page := TOGG.NewPage;
    if OggState.PageOut(OggPage) <> 1 then
    begin
      if len >= DecoderReadBufferSize then
        raise EVorbis.Create(sWrongOggStream);
    end;
    Ref^.ostr := TOGG.NewStream(OggPage.SerialNo);
    OggStream.PageIn(OggPage);
    Ref^.pack := TOGG.NewPacket;
    if not OggStream.PacketOut(OggPacket) then
      raise EVorbis.Create(sWrongHeader);

    fcomm := TVorbis.NewComment;
    if Decoder.HeaderIn(fcomm, OggPacket) < 0 then
      raise EVorbis.Create(sNotVorbis);

    i := 0;
    while i < 2 do
    begin
      while i < 2 do
      begin
        res := OggState.PageOut(OggPage);
        if (res = 0) then break; // Need more data
        { Don't complain about missing or corrupt data yet. We'll
          catch it at the packet output phase }
        if (res = 1) then begin
          OggStream.PageInIgnoreErrors(OggPage); { we can ignore any errors here
                                                   as they'll also become
                                                   apparent at packetout }
          while i < 2 do
          begin
            res := OggStream.PacketOutIgnoreErrors(OggPacket);
            if res = 0 then break;
            if res < 0 then
              raise EVorbis.Create(sCorruptSecondaryHeader);
            res := Decoder.HeaderIn(fcomm, OggPacket);
            if res < 0 then
              raise EVorbis.Create(sCorruptSecondaryHeader);
            Inc(i);
          end;
        end;
      end;
      { no harm in not checking before adding more }
      len := ReadDataToOgg;
      if (len = 0) and (i < 2) then
        raise EVorbis.Create(sUnexpectedEndOfFile);
    end;

    if not Decoder.Init then
      raise EVorbis.Create(sCantInitSynthesis);
    Ref^.block := Decoder.InitBlock;
  end else
    raise EVorbis.Create(sWrongOggStream);
end;

procedure TVorbisOggAltDecoder.Init;
begin
  FRef := GetMem(Sizeof(OggVorbisDec));
  FillByte(FRef^, Sizeof(OggVorbisDec), 0);

  Ref^.osyn := TOGG.NewSyncState;
  Ref^.dec  := TVorbisDecoder.CreateNotInit(TVorbis.NewInfo) as IVorbisDecoder;

  Ref^.state := 0;

  OggState.Init;
  ReadHeaders;
end;

procedure TVorbisOggAltDecoder.Done;
begin
  if Assigned(FRef) then
    FreeMemAndNil(FRef);
end;

function TVorbisOggAltDecoder.VorbisInfo : IVorbisInfo;
begin
  Result := Decoder.Info;
end;

function TVorbisOggAltDecoder.OggState : IOGGSyncState;
begin
  Result := Ref^.osyn;
end;

function TVorbisOggAltDecoder.OggStream : IOGGStreamState;
begin
  Result := Ref^.ostr;
end;

function TVorbisOggAltDecoder.OggPage : IOGGPage;
begin
  Result := Ref^.page;
end;

function TVorbisOggAltDecoder.OggPacket : IOGGPacket;
begin
  Result := Ref^.pack;
end;

function TVorbisOggAltDecoder.Decoder : IVorbisDecoder;
begin
  Result := Ref^.dec;
end;

class function TVorbisOggAltDecoder.DecoderReadBufferSize : Integer;
begin
  Result := 4096;
end;

function TVorbisOggAltDecoder.Ref : pOggVorbisDec;
begin
  Result := pOggVorbisDec(fRef);
end;

function TVorbisOggAltDecoder.DataMode : TOGGSoundDataMode;
begin
  Result := odmBytes;
end;

function TVorbisOggAltDecoder.ReadData(Buffer : Pointer; Sz : Integer;
  Par : Pointer) : Integer;
const OGG_DECODE_STATE_EOF = Byte(10);
      OGG_DECODE_STATE_NEXT_PAGE = Byte(0);
      OGG_DECODE_STATE_NEXT_PACKET = Byte(1);
      OGG_DECODE_STATE_NEXT_AUDIO_CHUNK = Byte(2);
var
  res, samples, bout, i, j : Integer;
  ptr : PInt16;
  ch  : pcfloat;
begin
  Result := 0;
  while (Result < Sz) and (Ref^.state <> OGG_DECODE_STATE_EOF) do
  begin
    case Ref^.state of
      OGG_DECODE_STATE_NEXT_PAGE : begin
         res := OggState.PageOut(OggPage);
         if res = 0 then begin
           if ReadDataToOgg <= 0 then
           begin
             Ref^.state := OGG_DECODE_STATE_EOF;
             break;
           end;
           Ref^.state := OGG_DECODE_STATE_NEXT_PAGE;
         end
         else
         if res < 0 then
           Ref^.state := OGG_DECODE_STATE_NEXT_PAGE
         else
         begin
           OggStream.PageInIgnoreErrors(OggPage);
           Ref^.state := OGG_DECODE_STATE_NEXT_PACKET;
         end;
      end;
      OGG_DECODE_STATE_NEXT_PACKET : begin
          res := OggStream.PacketOutIgnoreErrors(OggPacket);
          if res = 0 then
          begin
            Ref^.state := OGG_DECODE_STATE_NEXT_PAGE;
            if OggPage.EoS then
              Ref^.state := OGG_DECODE_STATE_EOF;
          end
          else
          if res < 0 then
            Ref^.state := OGG_DECODE_STATE_NEXT_PACKET
          else
          begin
            if Ref^.block.Synthesis(OggPacket) = 0 then
               Decoder.BlockIn(Ref^.block);
            Ref^.state := OGG_DECODE_STATE_NEXT_AUDIO_CHUNK;
          end;
      end;
      OGG_DECODE_STATE_NEXT_AUDIO_CHUNK : begin
          samples := Decoder.PCMOut(@(Ref^.pcm_buffer));
          if samples > 0 then
          begin
            res := (Sz - Result) div Channels div SizeOf(Int16);

            if res = 0 then Break;

            if samples < res then
              bout := samples else
              bout := res;

            { convert floats to 16 bit signed ints (host order) and
              interleave }
            for i := 0 to Channels-1 do
            begin
              ptr := @(PInt16(@(PByte(Buffer)[Result]))[i]);
              ch  := pcfloat(PPointer(Ref^.pcm_buffer)[i]);
              for j := 0 to bout-1 do
              begin
                res := Trunc(ch[j] * 32767.0 + 0.5);

                { might as well guard against clipping }
                if res > 32767 then
                  res := 32767;
                if res < -32768 then
                  res := -32768;

                ptr^ := res;
                Inc(ptr, Channels);
              end;
            end;

            Inc(Result, bout * Channels * Sizeof(int16));

            Decoder.Read(bout);
          end else
            Ref^.state := OGG_DECODE_STATE_NEXT_PACKET;
      end;
    else
      Break;
    end;
  end;
end;

procedure TVorbisOggAltDecoder.ResetToStart;
begin
  inherited ResetToStart;
end;

{ TVorbisOggStreamingAltDecoder }

function TVorbisOggStreamingAltDecoder.DoRead(_ptr : Pointer; _nbytes : Integer
  ) : Integer;
begin
  Result := fStream.Read(_ptr^, _nbytes);
end;

constructor TVorbisOggStreamingAltDecoder.Create(aStream : TStream);
begin
  fStream := aStream;
  inherited Create;
end;

procedure TVorbisOggStreamingAltDecoder.SetStream(aStream : TStream);
begin
  if fStream <> aStream then
  begin
    fStream := aStream;
    Ref^.state := 0;
  end;
end;

{ TVorbisOggStreamingFileDecoder }

function TVorbisOggStreamingFileDecoder.DoRead(_ptr : Pointer; _nbytes : Integer
  ) : Integer;
begin
  Result := fStream.Read(_ptr^, _nbytes);
end;

function TVorbisOggStreamingFileDecoder.DoSeek(_offset : Int64; _whence : Integer
  ) : Integer;
begin
  try
    with fStream do
      case _whence of
        0: Seek(_offset, soBeginning);
        1: Seek(_offset, soCurrent);
        2: Seek(_offset, soEnd);
      end;
    result := 0;
  except
    result := -1;
  end;
end;

function TVorbisOggStreamingFileDecoder.DoTell : Int64;
begin
  try
    result := fStream.Position;
  except
    result := -1;
  end;
end;

constructor TVorbisOggStreamingFileDecoder.Create(aStream : TStream);
begin
  fStream := aStream;
  inherited Create;
end;

procedure TVorbisOggStreamingFileDecoder.SetStream(aStream : TStream);
begin
  fStream := aStream;
end;

{ TVorbisOggStreamingEncoder }

procedure TVorbisOggStreamingEncoder.FlushOggStream;
begin
  OggStream.PagesOutToStream(fStream);
end;

constructor TVorbisOggStreamingEncoder.Create(aStream : TStream;
  aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq,
  aBitrate, aBitdepth : Cardinal; aQuality : Single;
  aComments : IOGGComment);
begin
  fStream := aStream;
  inherited Create(aMode, aChannels, aFreq, aBitrate, aBitdepth, aQuality, aComments);
end;

procedure TVorbisOggStreamingEncoder.SetStream(aStream : TStream);
begin
  fStream := aStream;
end;

procedure TVorbisOggStreamingEncoder.ReInitEncoder;
begin
  // reset encoder
  OggStream.Reset;
  Encoder.Init;
  fRef^.block := Encoder.InitBlock;
end;

{ TVorbisOggEncoder }

procedure TVorbisOggEncoder.Init(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
  aQuality : Single; aComments : IOGGComment);
begin
  fRef := GetMem(Sizeof(OggVorbisEnc));
  FillByte(fRef^, Sizeof(OggVorbisEnc), 0);

  fRef^.oggs := TOGG.NewStream(Abs(Random(Int64(Now))));
  if Assigned(aComments) then
  begin
    fComm := aComments;
  end else
    FComm := TVorbis.NewComment;
  fRef^.info := TVorbis.NewInfo;

  if aMode = oemVBR then
  begin
    fRef^.enc := TVorbis.NewEncoderVBR(fRef^.info, aChannels,
                                            aFreq, aQuality);
  end else
  if aMode = oemCBR then
  begin
    fRef^.enc := TVorbis.NewEncoderABR(fRef^.info, aChannels,
                                            aFreq, -1, aBitrate, -1);
  end;
  fRef^.block := Encoder.InitBlock;
  fRef^.bitdepth := aBitdepth;
  fRef^.mode := aMode;
end;

procedure TVorbisOggEncoder.Done;
begin
  if Assigned(fRef) then
  begin
    fRef^.block := nil;
    fRef^.enc := nil;
    fRef^.info := nil;
    FreeMemAndNil(fRef);
  end;
  fComm := nil;
end;

function TVorbisOggEncoder.GetBitdepth : Cardinal;
begin
  Result := fRef^.bitdepth;
end;

function TVorbisOggEncoder.GetBitrate : Cardinal;
begin
  Result := fRef^.info.BitrateNominal;
end;

function TVorbisOggEncoder.GetChannels : Cardinal;
begin
  Result := fRef^.info.Channels;
end;

function TVorbisOggEncoder.GetFrequency : Cardinal;
begin
  Result := fRef^.info.Rate;
end;

function TVorbisOggEncoder.GetMode : TOGGSoundEncoderMode;
begin
  Result := fRef^.mode;
end;

function TVorbisOggEncoder.GetQuality : Single;
begin
  Result := 0.0;
end;

function TVorbisOggEncoder.GetVersion : Integer;
begin
  Result := fRef^.info.Version;
end;

function TVorbisOggEncoder.OggStream : IOGGStreamState;
begin
  Result := fRef^.oggs;
end;

function TVorbisOggEncoder.Encoder : IVorbisEncoder;
begin
  Result := fRef^.enc;
end;

procedure TVorbisOggEncoder.FlushOggStream;
begin
  // do nothing
end;

function TVorbisOggEncoder.Ref : pOggVorbisEnc;
begin
  Result := fRef
end;

constructor TVorbisOggEncoder.Create(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal; aQuality : Single;
  aComments : IOGGComment);
begin
  Init(aMode, aChannels, aFreq, aBitrate, aBitdepth, aQuality,
              aComments);
end;

destructor TVorbisOggEncoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggEncoder.Comments : IOGGComment;
begin
  Result := fComm;
end;

function TVorbisOggEncoder.DataMode : TOGGSoundDataMode;
begin
  Result := odmSamples;
end;

function TVorbisOggEncoder.WriteData(Buffer : Pointer; Count : Integer;
  Par : Pointer) : Integer;
var
  BufW : Pointer;
begin
  if Count > 0 then
  begin
    BufW := Encoder.GetBuffer(Count);
    TVorbis.UninterleaveSamples(BufW, Buffer, Count, GetChannels, GetBitDepth);
    Encoder.Wrote(Count);

    Flush(Par);

    Result := Count;
  end else
    Result := 0;
end;

procedure TVorbisOggEncoder.WriteHeader(Par : Pointer);
var
  header,
  header_comm,
  header_code : IOGGPacket;
begin
  header := TOgg.NewPacket;
  header_comm := TOgg.NewPacket;
  header_code := TOgg.NewPacket;

  Encoder.HeaderOut(fComm,header,header_comm,header_code);

  OggStream.PacketIn(header);
  OggStream.PacketIn(header_comm);
  OggStream.PacketIn(header_code);

  FlushOggStream;
end;

procedure TVorbisOggEncoder.Close(Par : Pointer);
begin
  Encoder.Wrote(0);

  Flush(Par);
end;

procedure TVorbisOggEncoder.Flush(Par : Pointer);
var
  og : IOGGPacket;
begin
  og := TOgg.NewPacket;

  while (Encoder.BlockOut(fRef^.block) = 1) do
  begin
    fRef^.block.Analysis(nil);
    fRef^.block.BitrateAdd;

    while (Encoder.BitrateFlushPacket(og) <> 0) do
    begin
      OggStream.PacketIn(og);
      FlushOggStream;
    end;
  end;
end;

function TVorbisOggEncoder.Ready : Boolean;
begin
  Result := Assigned(fRef) and Assigned(fRef^.enc);
end;

{ TUniqVorbisInfo }

constructor TUniqVorbisInfo.Create;
begin
  FillByte(Fref, sizeof(FRef), 0);
  inherited Create(@FRef);
  Init;
end;

destructor TUniqVorbisInfo.Destroy;
begin
  Done;
  inherited Destroy;
end;

{ TVorbisState }

procedure TVorbisState.Done;
begin
  vorbis_dsp_clear(@FRef);
end;

constructor TVorbisState.Create(inf : IVorbisInfo);
begin
  FInfo := inf;
end;

destructor TVorbisState.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisState.InitBlock : IVorbisBlock;
begin
  Result := TVorbisBlock.Create as IVorbisBlock;
  vorbis_block_init(Ref, Result.Ref);
end;

function TVorbisState.Ref : pvorbis_dsp_state;
begin
  Result := @FRef;
end;

function TVorbisState.Info : IVorbisInfo;
begin
  Result := FInfo;
end;

{ TVorbisEncoder }

function TVorbisEncoder.Init : Boolean;
begin
  Result := vorbis_analysis_init(@FRef, Info.Ref) = 0;
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo);
begin
  inherited Create(inf);
  if not InitVBR(2, 48000, 0.5) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo; channels : Integer;
  rate : Int64; base_quality : Single);
begin
  inherited Create(inf);
  if not InitVBR(channels, rate, base_quality) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo; channels : Integer; rate,
  max_bitrate, nominal_bitrate, min_bitrate : Int64);
begin
  inherited Create(inf);
  if not InitABR(channels, rate, max_bitrate, nominal_bitrate, min_bitrate) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

function TVorbisEncoder.InitABR(channels : Integer; rate, max_bitrate,
  nominal_bitrate, min_bitrate : Int64) : Boolean;
begin
  Result := vorbis_encode_init(Info.Ref, channels, rate, max_bitrate, nominal_bitrate, min_bitrate) = 0;
end;

function TVorbisEncoder.InitVBR(channels : Integer; rate : Int64;
  base_quality : Single) : Boolean;
begin
  Result := vorbis_encode_init_vbr(Info.Ref, channels, rate, base_quality) = 0;
end;

function TVorbisEncoder.SetupManaged(channels : Integer; rate, max_bitrate,
  nominal_bitrate, min_bitrate : Int64) : Boolean;
begin
  Result := vorbis_encode_setup_managed(Info.Ref, channels, rate, max_bitrate,
                                        nominal_bitrate, min_bitrate) = 0;
end;

function TVorbisEncoder.SetupVBR(channels : Integer; rate : Int64;
  quality : Single) : Boolean;
begin
  Result := vorbis_encode_setup_vbr(Info.Ref, channels, rate, quality) = 0;
end;

function TVorbisEncoder.SetupInit : Boolean;
begin
  Result := vorbis_encode_setup_init(Info.Ref) = 0;
end;

procedure TVorbisEncoder.HeaderOut(vc : IOGGComment; header, header_comm,
  header_code : IOGGPacket);
begin
  vorbis_analysis_headerout(Ref,vc.Ref,header.Ref,header_comm.Ref,header_code.Ref);
end;

function TVorbisEncoder.GetBuffer(BufSize : Integer) : Pointer;
begin
  Result := vorbis_analysis_buffer(@FRef, BufSize);
end;

procedure TVorbisEncoder.Wrote(Sz : Integer);
begin
  vorbis_analysis_wrote(@FRef, Sz);
end;

function TVorbisEncoder.BlockOut(b : IVorbisBlock) : Integer;
begin
  Result := vorbis_analysis_blockout(@FRef, b.Ref);
end;

function TVorbisEncoder.BitrateFlushPacket(OggPac : IOGGPacket) : Integer;
begin
  Result := vorbis_bitrate_flushpacket(@FRef, OggPac.Ref);
end;

function TVorbisEncoder.Control(Number : Integer; Arg : pointer) : Integer;
begin
  Result := vorbis_encode_ctl(Info.Ref, Number, Arg);
end;

{ TVorbisBlock }

procedure TVorbisBlock.Done;
begin
  vorbis_block_clear(@FRef);
end;

function TVorbisBlock.Ref : pvorbis_block;
begin
  Result := @FRef
end;

constructor TVorbisBlock.Create;
begin
  FillByte(FRef, Sizeof(FRef), 0);
end;

destructor TVorbisBlock.Destroy;
begin
  Done;
  inherited Destroy;
end;

procedure TVorbisBlock.BitrateAdd;
begin
  vorbis_bitrate_addblock(@FRef);
end;

function TVorbisBlock.Analysis(OgPac : IOGGPacket) : integer;
begin
  if Assigned(OgPac) then
    Result := vorbis_analysis(@FRef, OgPac.Ref) else
    Result := vorbis_analysis(@FRef, nil);
end;

function TVorbisBlock.Synthesis(OgPac : IOGGPacket) : integer;
begin
  if Assigned(OgPac) then
    Result := vorbis_synthesis(@FRef, OgPac.Ref) else
    Result := vorbis_synthesis(@FRef, nil);
end;

function TVorbisBlock.TrackOnly(OgPac : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_trackonly(@FRef, OgPac.Ref);
end;

{ TRefVorbisInfo }

procedure TRefVorbisInfo.Init;
begin
  vorbis_info_init(FPRef);
end;

procedure TRefVorbisInfo.Done;
begin
  vorbis_info_clear(FPRef);
end;

function TRefVorbisInfo.Ref : pvorbis_info;
begin
  Result := FPRef
end;

constructor TRefVorbisInfo.Create(aRef : pvorbis_info);
begin
  FPRef := aRef;
end;

function TRefVorbisInfo.GetBitrateLower : Int64;
begin
  Result := FPRef^.bitrate_lower;
end;

function TRefVorbisInfo.GetBitrateNominal : Int64;
begin
  Result := FPRef^.bitrate_nominal;
end;

function TRefVorbisInfo.GetBitrateUpper : Int64;
begin
  Result := FPRef^.bitrate_upper;
end;

function TRefVorbisInfo.GetBitrateWindow : Int64;
begin
  Result := FPRef^.bitrate_window;
end;

function TRefVorbisInfo.GetChannels : Integer;
begin
  Result := FPRef^.channels;
end;

function TRefVorbisInfo.GetCodecSetup : pointer;
begin
  Result := FPRef^.codec_setup;
end;

function TRefVorbisInfo.GetRate : Int64;
begin
  Result := FPRef^.rate;
end;

function TRefVorbisInfo.GetVersion : Integer;
begin
  Result := FPRef^.version;
end;

procedure TRefVorbisInfo.SetBitrateLower(AValue : Int64);
begin
  FPRef^.bitrate_lower := AValue;
end;

procedure TRefVorbisInfo.SetBitrateNominal(AValue : Int64);
begin
  FPRef^.bitrate_nominal := AValue;
end;

procedure TRefVorbisInfo.SetBitrateUpper(AValue : Int64);
begin
  FPRef^.bitrate_upper := AValue;
end;

procedure TRefVorbisInfo.SetBitrateWindow(AValue : Int64);
begin
  FPRef^.bitrate_window := AValue;
end;

procedure TRefVorbisInfo.SetChannels(AValue : Integer);
begin
  FPRef^.channels := AValue;
end;

procedure TRefVorbisInfo.SetCodecSetup(AValue : pointer);
begin
  FPRef^.codec_setup := AValue;
end;

procedure TRefVorbisInfo.SetRate(AValue : Int64);
begin
  FPRef^.rate := AValue;
end;

procedure TRefVorbisInfo.SetVersion(AValue : Integer);
begin
  FPRef^.version := AValue;
end;

{ TRefVorbisComment }

procedure TRefVorbisComment.Init;
begin
  FRef := GetMem(Sizeof(vorbis_comment));
  FillByte(fRef^, Sizeof(vorbis_comment), 0);
  vorbis_comment_init(FRef);
end;

procedure TRefVorbisComment.Done;
begin
  if Assigned(FRef) then
  begin
    vorbis_comment_clear(FRef);
    FreeMemAndNil(FRef);
  end;
end;

function TRefVorbisComment.Ref : Pointer;
begin
  Result := FRef;
end;

constructor TRefVorbisComment.Create(aRef : pvorbis_comment);
begin
  fRef := aRef;
end;

procedure TRefVorbisComment.Add(const comment : String);
begin
  vorbis_comment_add(FRef, pcchar(pchar(comment)));
end;

procedure TRefVorbisComment.AddTag(const tag, value : String);
begin
  vorbis_comment_add_tag(@FRef, pcchar(pchar(tag)), pcchar(pchar(value)));
end;

function TRefVorbisComment.Query(const tag : String; index : integer) : String;
begin
  Result := StrPas( PChar(vorbis_comment_query(@FRef, pcchar(PChar(tag)), index)) );
end;

function TRefVorbisComment.QueryCount(const tag : String) : integer;
begin
  Result := vorbis_comment_query_count(@FRef, pcchar(PChar(tag)));
end;

{ TVorbis }

class function TVorbis.NewComment : IOGGComment;
begin
  Result := TUniqVorbisComment.Create as IOGGComment;
end;

class function TVorbis.RefComment(aRef : pvorbis_comment) : IOGGComment;
begin
  Result := TRefVorbisComment.Create(aRef) as IOGGComment;
end;

class function TVorbis.NewEncoder(inf : IVorbisInfo) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf) as IVorbisEncoder;
end;

class function TVorbis.NewEncoderVBR(inf : IVorbisInfo; channels : Integer;
  rate : Int64; base_quality : Single) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf, channels, rate, base_quality) as IVorbisEncoder;
end;

class function TVorbis.NewEncoderABR(inf : IVorbisInfo; channels : Integer;
  rate, max_bitrate, nominal_bitrate, min_bitrate : Int64) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf, channels,
                                       rate, max_bitrate,
                                       nominal_bitrate,
                                       min_bitrate) as IVorbisEncoder;
end;

class function TVorbis.NewInfo : IVorbisInfo;
begin
  Result := TUniqVorbisInfo.Create as IVorbisInfo;
end;

class function TVorbis.RefInfo(aRef : pvorbis_info) : IVorbisInfo;
begin
  Result := TRefVorbisInfo.Create(aRef) as IVorbisInfo;
end;

class function TVorbis.NewDecoder(inf : IVorbisInfo) : IVorbisDecoder;
begin
  Result := TVorbisDecoder.Create(inf) as IVorbisDecoder;
end;

class function TVorbis.NewOggStreamEncoder(aStream : TStream;
  aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq, aBitrate,
  aBitdepth : Cardinal; aQuality : Single; aComments : IOGGComment
  ) : TVorbisOggEncoder;
begin
  Result := TVorbisOggStreamingEncoder.Create(aStream, aMode, aChannels,
                                            aFreq,
                                            aBitrate,
                                            aBitdepth,
                                            aQuality,
                                            aComments);
end;

class function TVorbis.NewOggStreamFileDecoder(aStream : TStream
  ) : TVorbisOggDecoder;
begin
  Result := TVorbisOggStreamingFileDecoder.Create(aStream);
end;

class function TVorbis.NewOggStreamAltDecoder(aStream : TStream
  ) : TVorbisOggDecoder;
begin
  Result := TVorbisOggStreamingAltDecoder.Create(aStream);
end;

class procedure TVorbis.UninterleaveSamples(BufW, Buffer : Pointer; samples,
  Channels, BitDepth : integer);
var i : integer;
    fb : PPointer absolute BufW;
begin
  if BitDepth <> 16 then Exit;
  if Channels = 1 then
  begin
    for i:=0 to samples-1 do
    begin
      PSingle(fb[0])[i]:=Single(PSmallInt(Buffer)[i]) / 32768.0;
    end;
  end else
  if Channels = 2 then
  begin
    for i:=0 to samples-1 do
    begin
        PSingle(fb[0])[i]:=Single(PSmallInt(Buffer)[i*2])   / 32768.0;
        PSingle(fb[1])[i]:=Single(PSmallInt(Buffer)[i*2+1]) / 32768.0;
    end;
  end;
end;

class function TVorbis.VorbisLibsLoad(const aVorbisLibs : array of String
  ) : Boolean;
begin
  Result := InitVorbisInterface(aVorbisLibs);
end;

class function TVorbis.VorbisLibsLoadDefault : Boolean;
begin
  Result := InitVorbisInterface(VorbisDLL);
end;

class function TVorbis.IsVorbisLibsLoaded : Boolean;
begin
  Result := IsVorbisloaded;
end;

class function TVorbis.VorbisLibsUnLoad : Boolean;
begin
  Result := DestroyVorbisInterface;
end;

end.


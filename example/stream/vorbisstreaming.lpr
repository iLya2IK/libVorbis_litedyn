{
   VorbisStreaming example - part of libVorbis_dyn

   Copyright 2023 Ilya Medvedkov

   In this example, an vorbis-ogg file (named cInputFile) is opened and decoded
   into a data stream using vorbisfile library. The resulting stream is then
   re-encoded into a set of OGG Vorbis packets with specified duration. A set of
   packets is saved to a set of file on disk (cStreamFile[N]). A set of files
   is opened serially, decoded into a single data stream with alternative
   decoder (without vorbisfile) and saved in a new file in vorbis-ogg format
   (cOutputFile).

   step 1.
   cInputFile->VorbisOggDecoder->[pcm]->VorbisEncoder->[ogg packets]->cStreamFile[N]
   step 2.                       Files
   cStreamFile[N]->VorbisAltDecoder->[pcm]->VorbisOggEncoder->[ogg container]->cOutputFile
}

program vorbisstreaming;
uses
  {$ifdef LINUX}
  cthreads,
  {$endif}
  Classes, SysUtils,
  OGLVorbisWrapper, OGLOGGWrapper, OGLSoundUtils, OGLSoundUtilTypes;

const // the name of source vorbis-ogg file
      cInputFile  = '..' + PathDelim + 'media' + PathDelim + 'testing.ogg';
      // the name of the intermediate file with encoded packets in user`s format
      cStreamFile = '..' + PathDelim + 'media' + PathDelim + 'vorbispacket';
      cOGGs = '.oggs';
      // the name of dest reencoded vorbis-ogg file
      cOutputFile = '..' + PathDelim + 'media' + PathDelim + 'output.ogg';
      {$ifdef Windows}
      cVorbisDLL : Array [0..2] of String = ('..\libs\vorbis.dll',
                                             '..\libs\vorbisenc.dll',
                                             '..\libs\vorbisfile.dll');
      cOGGDLL : String = ('..\libs\ogg.dll');
      {$endif}
      // duration of data chunk to encode
      cChunckDuration : Integer = 1000; // 1 second
      cChunckSize     : Integer = 4096;
      cHeaderEveryFrame : Boolean = false; // true - every file is a complete
                                           //        ogg-stream with headers and
                                           //        eos packet
                                           // false - each file is just a
                                           //         fragment of an ogg stream.
                                           //         the headers are only in
                                           //         the first file

var
  oggf : TVorbisFile; // interface to encode/decode vorbis-Ogg data
  pack_enc : TVorbisOggEncoder;  // Vorbis custom streaming encoder
  pack_dec : TVorbisOggDecoder;  // Vorbis custom streaming decoder
  aFileStream : TFileStream;     // TFileStream linked to cStreamFile
  Buffer : Pointer;              // intermediate buffer
  len : ISoundFrameSize;         // length of data
  MaxLength,
  ChunkLength,
  frame_len : ISoundFrameSize;   // total length of frame
  i,  bitrate : Integer;         // current bitrate
  fEOF  : Boolean;
  Files : TStringList;
  aFile : String;

  aEncProps : ISoundEncoderProps; // encoder properties
begin
  // Initialize vorbis, vorbisenc, vorbisfile interfaces - load libraries
  {$ifdef Windows}
  if TVorbis.VorbisLibsLoad(cVorbisDLL) and TOGG.OGGLibsLoad([cOGGDLL]) then
  {$else}
  if TVorbis.VorbisLibsLoadDefault and TOGG.OGGLibsLoadDefault then
  {$endif}
  begin
    Files := TStringList.Create;
    // Create vorbis-Ogg encoder/decoder interface
    oggf := TVorbisFile.Create;
    try
      // Config TvorbisFile to decoder state (vorbisfile mode)
      if oggf.LoadFromFile(cInputFile, false) then
      begin
        // cInputFile opended and headers/coments are loaded
        MaxLength := oggf.Decoder.FrameFromDuration(cChunckDuration);
        ChunkLength := oggf.Decoder.FrameFromBytes(cChunckSize);
        // get the file bitrate from vorbis-ogg decoder
        bitrate := oggf.Decoder.Bitrate;

        // gen encoder properties
        // quality = 0.5
        aEncProps := TOGLSound.EncProps([TOGLSound.PROP_MODE, oemVBR,
                                         TOGLSound.PROP_CHANNELS, oggf.Channels,
                                         TOGLSound.PROP_FREQUENCY, oggf.Frequency,
                                         TOGLSound.PROP_BITRATE, bitrate,
                                         TOGLSound.PROP_SAMPLE_SIZE, ss16bit,
                                         TOGLSound.PROP_QUALITY, 0.5]);

        // initialize intermediate buffer to store decoded data chunk
        Buffer := GetMem(cChunckSize);
        try
          // initialize custom streaming encoder
          pack_enc := TVorbis.NewOggStreamEncoder(nil, [sdpForceNotSeekable],
                                                       aEncProps, nil);
          try
            fEOF := false;
            while not fEOF do
            begin
              aFile := cStreamFile + Format('%.4d', [Files.Count + 1]) + cOGGs;
              Files.Add(aFile);

              aFileStream := TFileStream.Create(aFile, fmOpenWrite or fmCreate);
              try
                TVorbisOggStreamingEncoder(pack_enc).SetStream(aFileStream);
                if (Files.Count = 1) then
                  pack_enc.WriteHeader(nil)
                else
                begin
                  if cHeaderEveryFrame then
                  begin
                    // alternatively recreate pac_enc here:
                    // FreeAndNil(pack_enc);
                    // pack_enc := TVorbis.NewOggStreamEncoder(...
                    TVorbisOggStreamingEncoder(pack_enc).ReInitEncoder;
                    pack_enc.WriteHeader(nil);
                  end;
                end;

                frame_len := TOGLSound.NewEmptyFrame(MaxLength);
                repeat
                  // read decoded pcm data from vorbis-ogg file
                  // len - length of decoded data in bytes
                  len := oggf.ReadData(Buffer, ChunkLength, nil);

                  if len.IsValid then
                  begin
                    frame_len.Inc(len);
                    // this is where pcm data is encoded into the vorbis packets.
                    pack_enc.WriteData(Buffer, len, nil);
                  end else
                    fEOF := true;
                until len.Less(ChunkLength) or
                      frame_len.GreaterOrEqual(MaxLength) or fEOF;

                if fEOF or cHeaderEveryFrame then
                  // complete the stream formation process.
                  // write the packets that are in the cache.
                  pack_enc.Close(nil) else
                  pack_enc.Flush(nil);
              finally
                aFileStream.Free;
              end;
            end;
          finally
            pack_enc.Free;
          end;
        finally
          FreeMemAndNil(Buffer);
        end;

        // Config TVorbisFile to encode state (vorbisenc mode)
        // and create/open to write cOutputFile
        // quality = 0.5
        if oggf.SaveToFile(cOutputFile, aEncProps, nil) then
        begin
          // cOutputFile has been created/opened and headers/comments have
          // been
          // initialize intermediate buffer to store decoded data chunk
          Buffer := GetMem(cChunckSize);
          try
            pack_dec := nil;
            // initialize custom streaming decoder
            For i := 0 to Files.Count-1 do
            begin
              // open file stream to read from cStreamFile
              aFileStream := TFileStream.Create(Files[i], fmOpenRead);
              if Assigned(pack_dec) then
              begin
                if cHeaderEveryFrame then
                begin
                   FreeAndNil(pack_dec);
                   pack_dec := TVorbis.NewOggStreamAltDecoder(aFileStream)
                end else
                   TVorbisOggStreamingAltDecoder(pack_dec).SetStream(aFileStream);
              end else
                pack_dec := TVorbis.NewOggStreamAltDecoder(aFileStream);
              try
                repeat
                  // read decoded pcm data from vorbis streaming file
                  len := pack_dec.ReadData(Buffer, ChunkLength, nil);

                  if len.IsValid then begin
                    // this is where pcm data samples are encoded into the
                    // vorbis-ogg format and then written to the vorbis-ogg file.
                    oggf.WriteData(Buffer, len, nil);
                  end;
                until len.Less(ChunkLength);
              finally
                aFileStream.Free;
              end;
            end;
          finally
            FreeMemAndNil(Buffer);
            if Assigned(pack_dec) then FreeAndNil(pack_dec);
          end;
          // complete the ogg stream formation process.
          // write the ogg data that is in the cache.
          oggf.StopStreaming;
        end;
      end;
    finally
      oggf.Free;
      Files.Free;
    end;
    // close vorbis interfaces
    TVorbis.VorbisLibsUnLoad;
    TOGG.OGGLibsUnLoad;
  end else
    WriteLn('Cant load libraries');
end.

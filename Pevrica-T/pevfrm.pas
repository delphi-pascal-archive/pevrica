unit pevfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, inifiles;

type
  TForm1 = class (TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click (Sender: TObject);
    procedure Button2Click (Sender: TObject);
    procedure Button3Click (Sender: TObject);
    procedure Button4Click (Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses  pevcoder, mwHashStrings;


{$R *.dfm}


procedure TForm1.Button1Click (Sender: TObject);
  var
    IM: TPevCoder;
    OD: TOpenDialog;
    SD: TSaveDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Filter := '*|*';
    if OD.Execute then
      begin

      SD := TSaveDialog.Create(nil);
      SD.Filter := '*|*';
      if SD.Execute then
        begin

        IM := TPevCoder.Create;
        IM.LoadDictionary(ExtractFilePath(Application.ExeName) + 'index.idx');

        IM.LoadToBA(od.FileName);
        IM.SavePacked(sd.FileName);
        IM.Free;

        end;
      SD.Free;

      end;
    OD.Free;

  end;

procedure TForm1.Button2Click (Sender: TObject);
  var
    IM: TPevCoder;
    OD: TOpenDialog;
    SD: TSaveDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Filter := '*|*';
    if OD.Execute then
      begin

      SD := TSaveDialog.Create(nil);
      SD.Filter := '*|*';
      if SD.Execute then
        begin

        IM := TPevCoder.Create;
        IM.LoadDictionary(ExtractFilePath(Application.ExeName) + 'index.idx');
        IM.LoadPacked(OD.FileName);
        IM.SaveToBA(SD.FileName);
        IM.Free;

        end;
      SD.Free;

      end;
    OD.Free;

  end;


procedure LoadPreprocess (fn: string);
  var
    TF: TFileStream;
    SL: TmwHashedStringList;
    SL2: THashedStringList;
    word: string;
    I: integer;

  var
    texfiletmp: string;
  var
    texfile: string;


  procedure addword (s: string);
    begin
      if length(s) = 3 then
        if length(s) <= 14 then
          begin

          if Sl.IndexOf(s) = -1 then
            begin

            if SL2.Count > 0 then
              begin
              if SL2.IndexOf(s) = -1 then
                Sl2.Add(s);
              end
            else
              SL2.Add(s);
            end;

          end;

    end;

  begin

    SL := TmwHashedStringList.Create;
    SL.Capacity := 262143;
    SL.CaseSensitive := True;
    SL.LoadFromFile('c:\coding\pevrica_t\Index.idx');

    SL2 := THashedStringList.Create;
    SL2.CaseSensitive := True;


    TF := TFileStream.Create(fn, fmOpenRead);

    SetLength(TexFileTMP, TF.Size);

    TF.Read(TexFileTMP[1], TF.Size);

    TF.Free;


    TexFile := '';

    word := '';


    for I := 1 to length(texfiletmp) do

      begin

      if True then

        begin

        case texfiletmp[i] of

          'a'..'z', 'A'..'Z', 'à'..'ÿ', 'À'..'ß':

            begin

            word := word + texfiletmp[i];

            end
          else

            begin
            if word <> '' then
              begin
              AddWord(word);
              word := '';
              end;

            texfile := texfile + texfiletmp[i];
            end;

          end;

        end;

      end;

    SL.Free;
    SL2.Sort;
    SL2.SaveToFile('c:\coding\pevrica_t\Index2.idx');
    SL2.Free;

  end;


procedure TForm1.Button3Click (Sender: TObject);
  var
    sl, sl2: TStringList;
    i: integer;
    s: string;
  begin

    sl := TStringList.Create;
    sl.Capacity := 2560000;
    sl.LoadFromFile('c:\coding\pevrica_t\Index2.idx');
    sl2 := TStringList.Create;

    for I := 0 to Sl.Count - 1 do
      begin
      if length(sl[i]) = 3 then
        if pos(' ', sl[i]) = 0 then
          if pos('-', sl[i]) = 0 then
            if pos('(', sl[i]) = 0 then
              if pos(')', sl[i]) = 0 then
                if pos('2', sl[i]) = 0 then
                  begin
                  sl2.Add(sl[i]);
                  s := sl[i];
                  s[1] := UpCase(s[1]);
                  sl2.Add(s);
                  end;
      end;
    sl.Free;
    sl2.SaveToFile('c:\coding\pevrica_t\Index1.idx');
    sl2.Free;
  end;

procedure TForm1.Button4Click (Sender: TObject);
  begin
    LoadPreprocess('c:\coding\pevrica_t\23680.txt');
  end;

end.

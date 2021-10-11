unit pevfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class (TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click (Sender: TObject);
    procedure Button2Click (Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses  pevcoder;


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

        IM.LoadPacked(OD.FileName);
        IM.SaveToBA(SD.FileName);
        IM.Free;

        end;
      SD.Free;

      end;
    OD.Free;

  end;

end.

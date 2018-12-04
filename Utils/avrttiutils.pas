unit avRTTIUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils;

procedure WritePersistent(AStream: TStream; APersistent: TPersistent);
procedure ReadPersistent (AStream: TStream; APersistent: TPersistent);

implementation

type
  TCA = class(TComponent)
  private
    FP: TPersistent;
  public
    procedure AutoSkipProperty(Reader: TReader; Instance: TPersistent; var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
    procedure AutoHandleError(Reader: TReader; const Message: string; var Handled: Boolean);
  published
    property P: TPersistent read FP write FP;
  end;

procedure WritePersistent(AStream: TStream; APersistent: TPersistent);
var
  cmp: TCA;
begin
  cmp := TCA.Create(nil);
  try
    cmp.P := APersistent;
    AStream.WriteComponent(cmp);
  finally
    FreeAndNil(cmp);
  end;
end;

procedure ReadPersistent(AStream: TStream; APersistent: TPersistent);
var cmp: TCA;
    Reader: TReader;
begin
  Reader := nil;
  cmp := nil;
  try
    cmp := TCA.Create(nil);
    Reader := TReader.Create(AStream, 4096);
    {$IfDef FPC}
    Reader.OnPropertyNotFound := @cmp.AutoSkipProperty;
    Reader.OnError := @cmp.AutoHandleError;
    {$Else}
    Reader.OnError := cmp.AutoHandleError;
    {$EndIf}
    cmp.P := APersistent;
    Reader.ReadRootComponent(cmp);
  finally
    FreeAndNil(cmp);
    FreeAndNil(Reader);
  end;
end;

{ TCA }

procedure TCA.AutoSkipProperty(Reader: TReader; Instance: TPersistent; var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Handled := True;
  Skip := True;
end;

procedure TCA.AutoHandleError(Reader: TReader; const Message: string; var Handled: Boolean);
begin
  Handled := True;
end;

end.


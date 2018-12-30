unit ServiceIntf;

interface

uses
  SynCommons,
  mORMot,
  System.SysUtils,
  System.Generics.Collections;

type
  INotifyCallback = interface(IInvokable)
    ['{EA7EFE51-3EBA-4047-A356-253374518D1D}']
    procedure Init(const GroupValueCount, ExportGroupValueCount, MaxGroupCount: Cardinal);
    procedure GroupCodeName(const FirstRow: Cardinal);
    procedure Finish;
  end;

  IService = interface(IServiceWithCallbackReleased)
    ['{C92DCBEA-C680-40BD-8D9C-3E6F2ED9C9CF}']
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
    procedure InitFinish(const ID: string; const ProcessorCount: Cardinal);
    procedure TaskFinish(const ID: string; const FirstRow: Cardinal; const CodeName: string);
    procedure TaskFailed(const ID: string; const FirstRow: Cardinal);
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IService), TypeInfo(INotifyCallback)]);

end.

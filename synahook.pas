unit SynaHook;

interface

type
  THookReason = (
    HR_connect,
    HR_login,
    HR_logout,
    HR_command,
    HR_result,
    HR_beginTransfer,
    HR_endTransfer,
    HR_TransferCounter
    );

  THookEvent = procedure(Sender: TObject; Reason: THookReason; Value: string) of object;

implementation

end.

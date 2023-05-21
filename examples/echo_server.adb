with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Network.Addresses;
with Network.Managers.TCP_V4;

with Echo_Listeners;

procedure Echo_Server is
   Addr : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("/ip4/0.0.0.0/tcp/4001");

   Manager  : Network.Managers.Manager;
   Listener : aliased Echo_Listeners.Connection_Listener;
   Error    : League.Strings.Universal_String;
begin
   Manager.Initialize;
   Network.Managers.TCP_V4.Register (Manager);

   Manager.Listen
     (List     => (1 => Network.Addresses.To_Address (Addr)),
      Listener => Listener'Unchecked_Access,
      Error    => Error);

   if not Error.Is_Empty then
      Ada.Wide_Wide_Text_IO.Put ("Listen error: ");
      Ada.Wide_Wide_Text_IO.Put_Line (Error.To_Wide_Wide_String);
      return;
   end if;

   loop
      Manager.Wait (1.0);
   end loop;
end Echo_Server;

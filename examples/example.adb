--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Application;

with Network.Managers;
with Network.Managers.TCP_V4;
with Network.Connections;
with Network.Addresses;

with Listeners;

procedure Example is
   Addr : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Error : League.Strings.Universal_String;
   Manager : Network.Managers.Manager;
   Connection : Network.Connections.Connection_Access;
begin
   Manager.Initialize;
   Network.Managers.TCP_V4.Register (Manager);

   Manager.Connect
     (Address => Network.Addresses.To_Address (Addr),
      Error   => Error,
      Result  => Connection);

   if not Error.Is_Empty then
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Connect fails: " & Error.To_Wide_Wide_String);
      return;
   end if;

   declare
      Listener : aliased Listeners.Listener (Connection);
   begin
      Connection.Set_Listener (Listener'Unchecked_Access);

      for J in 1 .. 100 loop
         Manager.Wait (1.0);
         exit when Connection.Is_Closed;
      end loop;
   end;
end Example;

--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Application;

with Network.Addresses;
with Network.Connection_Promises;
with Network.Managers.TCP_V4;
with Network.Managers;

with Listeners;

procedure Example is
   Addr : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Error   : League.Strings.Universal_String;
   Manager : Network.Managers.Manager;
   Promise : Network.Connection_Promises.Promise;
begin
   Manager.Initialize;
   Network.Managers.TCP_V4.Register (Manager);

   Manager.Connect
     (Address => Network.Addresses.To_Address (Addr),
      Error   => Error,
      Promise => Promise);

   if not Error.Is_Empty then
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Connect fails: " & Error.To_Wide_Wide_String);
      return;
   end if;

   declare
      Listener : aliased Listeners.Listener :=
        (Promise => Promise, others => <>);
   begin
      Promise.Add_Listener (Listener'Unchecked_Access);

      for J in 1 .. 100 loop
         Manager.Wait (1.0);
         exit when Listener.Done;
      end loop;
   end;
end Example;

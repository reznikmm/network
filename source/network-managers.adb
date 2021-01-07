--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Network.Managers is

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self    : in out Manager'Class;
      Address : Network.Addresses.Address;
      Error   : out League.Strings.Universal_String;
      Result  : out Network.Connections.Connection_Access;
      Options : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
   begin
      for Proto of Self.Proto (1 .. Self.Last) loop
         if Proto.Can_Connect (Address) then
            Proto.Connect (Address, Self.Poll, Error, Result, Options);

            return;
         end if;
      end loop;

      Error.Append ("Unknown protocol");
   end Connect;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Manager'Class) is
   begin
      Self.Poll.Initialize;
   end Initialize;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Self     : in out Manager'Class;
      List     : Network.Addresses.Address_Array;
      Listener : Connection_Listener_Access;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
      Done : array (List'Range) of Boolean := (List'Range => False);
   begin
      for Proto of Self.Proto (1 .. Self.Last) loop
         declare
            Ok    : League.Strings.Universal_String;
            Slice : Network.Addresses.Address_Array (List'Range);
            Last  : Natural := Slice'First - 1;
         begin
            for J in List'Range loop
               if not Done (J) and then Proto.Can_Listen (List (J)) then
                  Done (J) := True;
                  Last := Last + 1;
                  Slice (Last) := List (J);
               end if;
            end loop;

            if Last >= Slice'First then
               Proto.Listen
                 (Slice (Slice'First .. Last),
                  Listener,
                  Self.Poll,
                  Ok,
                  Options);

               Error.Append (Ok);
            end if;
         end;
      end loop;

      if Done /= (List'Range => True) then
         for J in Done'Range loop
            Error.Append ("Unknown protocol for ");
            Error.Append (Network.Addresses.To_String (List (J)));
            Error.Append (". ");
         end loop;
      end if;
   end Listen;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self     : in out Manager;
      Protocol : not null Protocol_Access) is
   begin
      Self.Last := Self.Last + 1;
      Self.Proto (Self.Last) := Protocol;
   end Register;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Self    : in out Manager'Class;
      Timeout : Duration) is
   begin
      Self.Poll.Wait (Timeout);
   end Wait;

end Network.Managers;

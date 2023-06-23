--  SPDX-FileCopyrightText: 2021-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Network.Connections.Internal;

package body Network.Managers is

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self     : in out Manager'Class;
      Address  : Network.Addresses.Address;
      Error    : out League.Strings.Universal_String;
      Listener : not null Connection_Listener_Access;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
   begin
      for Proto of Self.Proto (1 .. Self.Last) loop
         if Proto.Can_Connect (Address) then
            Proto.Connect (Address, Error, Listener, Options);

            return;
         end if;
      end loop;

      Error.Append ("Unknown protocol");
   end Connect;

   -----------------------
   -- Delete_Connection --
   -----------------------

   procedure Delete_Connection
     (Self       : in out Manager;
      Connection : not null access
        Network.Abstract_Connections.Abstract_Connection'Class) is
   begin
      Self.Deleted.Append (Connection);
   end Delete_Connection;

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
      Listener : not null Connection_Listener_Access;
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

   --------------------
   -- New_Connection --
   --------------------

   procedure New_Connection
     (Self       : in out Manager;
      Connection : not null access
        Network.Abstract_Connections.Abstract_Connection'Class) is
   begin
      Connection.Reference;
   end New_Connection;

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

      --  Clean up closed connections
      for J of Self.Deleted loop
         declare
            Connection : Network.Connections.Connection :=
              Network.Connections.Internal.Cast (J);
            pragma Unreferenced (Connection);
            Ignore : Boolean;
         begin
            Ignore := J.Dereference;
            --  Now Connection will be destroyed if there are no references
         end;
      end loop;

      Self.Deleted.Clear;
   end Wait;

end Network.Managers;

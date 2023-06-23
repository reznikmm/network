--  SPDX-FileCopyrightText: 2021-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Interfaces.C;

with Network.Managers.TCP_V4_Out;

package body Network.Managers.TCP_V4 is

   subtype Out_Socket_Access is TCP_V4_Out.Out_Socket_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (TCP_V4_Listen.Listen_Socket, Listen_Socket_Access);

   function Message (E : Ada.Exceptions.Exception_Occurrence)
     return League.Strings.Universal_String is
      (League.Strings.From_UTF_8_String
        (Ada.Exceptions.Exception_Message (E)));

   -----------------
   -- Can_Connect --
   -----------------

   overriding function Can_Connect
     (Self    : Protocol;
      Address : Network.Addresses.Address) return Boolean
   is
      pragma Unreferenced (Self);

      List : constant League.String_Vectors.Universal_String_Vector :=
        Network.Addresses.To_String (Address).Split ('/');
   begin
      return List.Length = 5 and then
        List (1).Is_Empty and then
        List (2).To_Wide_Wide_String = "ip4" and then
        List (4).To_Wide_Wide_String = "tcp";
   end Can_Connect;

   ----------------
   -- Can_Listen --
   ----------------

   overriding function Can_Listen
     (Self : Protocol; Address : Network.Addresses.Address) return Boolean
       renames Can_Connect;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Self     : in out Protocol;
      Address  : Network.Addresses.Address;
      Error    : out League.Strings.Universal_String;
      Listener : not null Connection_Listener_Access;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
      pragma Unreferenced (Options);

      Req  : GNAT.Sockets.Request_Type :=
        (GNAT.Sockets.Non_Blocking_IO, Enabled => True);

      List : constant League.String_Vectors.Universal_String_Vector :=
        Network.Addresses.To_String (Address).Split ('/');

      Internal : GNAT.Sockets.Socket_Type;
      Addr     : GNAT.Sockets.Sock_Addr_Type;
      Socket   : Out_Socket_Access;
      Ignore   : Boolean;
   begin
      Addr.Addr := GNAT.Sockets.Inet_Addr (List (3).To_UTF_8_String);
      Addr.Port := GNAT.Sockets.Port_Type'Wide_Wide_Value
        (List (5).To_Wide_Wide_String);

      GNAT.Sockets.Create_Socket (Internal);
      GNAT.Sockets.Control_Socket (Internal, Req);

      begin
         GNAT.Sockets.Connect_Socket (Internal, Addr);
      exception  --  Ignore Operation_Now_In_Progress
         when E : GNAT.Sockets.Socket_Error =>
            declare
               Kind : constant GNAT.Sockets.Error_Type :=
                 GNAT.Sockets.Resolve_Exception (E);
            begin
               if Kind not in GNAT.Sockets.Operation_Now_In_Progress then
                  Error := Message (E);
                  return;
               end if;
            end;
      end;

      Socket := new TCP_V4_Out.Out_Socket (Self.Manager);
      Socket.Internal := Internal;
      Socket.Events := (Network.Polls.Output => True, others => False);
      Socket.Callback := Listener;
      Self.Manager.New_Connection (Socket);
      Ignore := Socket.Dereference;

      Self.Manager.Poll.Watch
        (Interfaces.C.int (GNAT.Sockets.To_C (Internal)),
         Events   => Socket.Events,
         Listener => Socket.all'Access);

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Error := Message (E);
   end Connect;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Self     : in out Protocol;
      List     : Network.Addresses.Address_Array;
      Listener : not null Connection_Listener_Access;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
      pragma Unreferenced (Options);

      Sockets : array (List'Range) of Listen_Socket_Access :=
        (others => new TCP_V4_Listen.Listen_Socket (Self.Manager));
   begin
      for Index in List'Range loop
         declare
            Address : Network.Addresses.Address renames List (Index);
            Socket  : Listen_Socket_Access renames Sockets (Index);
            Part    : constant League.String_Vectors.Universal_String_Vector :=
              Network.Addresses.To_String (Address).Split ('/');
            Addr    : GNAT.Sockets.Sock_Addr_Type;
         begin
            Addr.Addr := GNAT.Sockets.Inet_Addr (Part (3).To_UTF_8_String);

            Addr.Port := GNAT.Sockets.Port_Type'Wide_Wide_Value
              (Part (5).To_Wide_Wide_String);

            GNAT.Sockets.Create_Socket (Socket.Internal);
            GNAT.Sockets.Bind_Socket (Socket.Internal, Addr);
            GNAT.Sockets.Listen_Socket (Socket.Internal);
            Socket.Listener := Listener;
         exception
            when E : GNAT.Sockets.Socket_Error =>
               Error.Append (Network.Addresses.To_String (Address));
               Error.Append (" => ");
               Error.Append (Message (E));
         end;
      end loop;

      if Error.Is_Empty then
         for Socket of Sockets loop
            Socket.Events := (Network.Polls.Input => True, others => False);

            Self.Manager.Poll.Watch
              (Interfaces.C.int (GNAT.Sockets.To_C (Socket.Internal)),
               Events   => Socket.Events,
               Listener => Socket.all'Access);

            Self.Listen.Append (Socket);
         end loop;
      else
         for Socket of Sockets loop
            GNAT.Sockets.Close_Socket (Socket.Internal);
            Free (Socket);
         end loop;
      end if;
   end Listen;

   --------------
   -- Register --
   --------------

   procedure Register (Manager : in out Network.Managers.Manager) is
   begin
      Manager.Register (new Protocol (Manager'Unchecked_Access));
   end Register;

   ------------
   -- Remote --
   ------------

   function Remote (Value : GNAT.Sockets.Sock_Addr_Type)
     return Network.Addresses.Address
   is
      Result : League.Strings.Universal_String;
   begin
      Result.Append ("/ip4/");
      Result.Append
        (League.Strings.From_UTF_8_String (GNAT.Sockets.Image (Value.Addr)));
      Result.Append ("/tcp/");

      declare
         Port : constant Wide_Wide_String := Value.Port'Wide_Wide_Image;
      begin
         Result.Append (Port (2 .. Port'Last));

         return Network.Addresses.To_Address (Result);
      end;

   exception
      when GNAT.Sockets.Socket_Error =>
         return Network.Addresses.To_Address
           (League.Strings.Empty_Universal_String);
   end Remote;

end Network.Managers.TCP_V4;

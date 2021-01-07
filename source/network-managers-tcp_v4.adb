--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Exceptions;
with Ada.Streams;

with GNAT.Sockets;
with Interfaces.C;

package body Network.Managers.TCP_V4 is

   type Out_Socket (Poll : Network.Polls.Poll_Access) is
     new Network.Polls.Listener
       and Network.Connections.Connection with
   record
      Listener   : Network.Connections.Event_Listener_Access;
      --  if Listener = null then Is_Closed = True
      Error      : League.Strings.Universal_String;
      Internal   : GNAT.Sockets.Socket_Type;
      Events     : Network.Polls.Event_Set := (others => False);
      Is_Closed  : Boolean := True;  --  Not open or closed already
      Connecting : Boolean := True;  --  In connection phase
      In_Event   : Boolean := False;  --  Inside On_Event
   end record;

   type Out_Socket_Access is access all Out_Socket;

   overriding function Is_Closed (Self : Out_Socket) return Boolean;

   overriding procedure Set_Listener
     (Self     : in out Out_Socket;
      Listener : Network.Connections.Event_Listener_Access);

   overriding procedure On_Event
     (Self   : in out Out_Socket;
      Events : Network.Polls.Event_Set);

   overriding procedure Read
     (Self : in out Out_Socket;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out Out_Socket;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Close (Self : in out Out_Socket);

   procedure Change_Watch
     (Self : in out Out_Socket'Class;
      Set  : Network.Polls.Event_Set);

   use type Network.Polls.Event_Set;

   Write_Event : constant Network.Polls.Event_Set :=
     (Network.Polls.Output => True, others => False);

   Read_Event : constant Network.Polls.Event_Set :=
     (Network.Polls.Input => True, others => False);

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

   ------------------
   -- Change_Watch --
   ------------------

   procedure Change_Watch
     (Self : in out Out_Socket'Class;
      Set  : Network.Polls.Event_Set)
   is
   begin
      Self.Poll.Change_Watch
        (Set,
         Interfaces.C.int (GNAT.Sockets.To_C (Self.Internal)),
         Self'Unchecked_Access);
      Self.Events := Set;
   end Change_Watch;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Self : in out Out_Socket) is
   begin
      if not Self.Is_Closed then
         Self.Change_Watch ((others => False));
         GNAT.Sockets.Close_Socket (Self.Internal);
         Self.Is_Closed := True;
      end if;
   end Close;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Self    : in out Protocol;
      Address : Network.Addresses.Address;
      Poll    : in out Network.Polls.Poll;
      Error   : out League.Strings.Universal_String;
      Result  : out Network.Connections.Connection_Access;
      Options : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
      pragma Unreferenced (Options, Self);

      Req  : GNAT.Sockets.Request_Type :=
        (GNAT.Sockets.Non_Blocking_IO, Enabled => True);

      List : constant League.String_Vectors.Universal_String_Vector :=
        Network.Addresses.To_String (Address).Split ('/');

      Internal : GNAT.Sockets.Socket_Type;
      Addr     : GNAT.Sockets.Sock_Addr_Type;
      Socket   : Out_Socket_Access;
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
                  Error := League.Strings.From_UTF_8_String
                    (Ada.Exceptions.Exception_Message (E));
                  return;
               end if;
            end;
      end;

      Socket := new Out_Socket (Poll'Unchecked_Access);
      Socket.Internal := Internal;

      Poll.Watch
        (Interfaces.C.int (GNAT.Sockets.To_C (Internal)),
         Events   => (Network.Polls.Output => True, others => False),
         Listener => Socket.all'Access);

      Result := Socket.all'Access;
   exception
      when E : GNAT.Sockets.Socket_Error =>
         Error := League.Strings.From_UTF_8_String
           (Ada.Exceptions.Exception_Message (E));
   end Connect;

   ---------------
   -- Is_Closed --
   ---------------

   overriding function Is_Closed (Self : Out_Socket) return Boolean is
   begin
      return Self.Is_Closed;
   end Is_Closed;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Self     : in out Protocol;
      List     : Network.Addresses.Address_Array;
      Listener : Connection_Listener_Access;
      Poll     : in out Network.Polls.Poll;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector)
   is
      pragma Unreferenced (List, Poll, Error, Options);
   begin
      raise Program_Error;
   end Listen;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Out_Socket;
      Events : Network.Polls.Event_Set)
   is
      use type Network.Connections.Event_Listener_Access;

      function Get_Error return League.Strings.Universal_String;
      procedure Disconnect (Error : League.Strings.Universal_String);

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect (Error : League.Strings.Universal_String) is
      begin
         Self.Change_Watch ((others => False));
         GNAT.Sockets.Close_Socket (Self.Internal);
         Self.Is_Closed := True;

         if Self.Listener /= null then
            Self.Listener.Closed (Error);
         end if;
      end Disconnect;

      ---------------
      -- Get_Error --
      ---------------

      function Get_Error return League.Strings.Universal_String is
         use type GNAT.Sockets.Error_Type;

         Option : constant GNAT.Sockets.Option_Type :=
           GNAT.Sockets.Get_Socket_Option
             (Self.Internal,
              GNAT.Sockets.Socket_Level,
              GNAT.Sockets.Error);
      begin
         if Option.Error = GNAT.Sockets.Success then
            return League.Strings.Empty_Universal_String;
         else
            return League.Strings.To_Universal_String
              (GNAT.Sockets.Error_Type'Wide_Wide_Image (Option.Error));
         end if;
      end Get_Error;

      Prev : constant Network.Polls.Event_Set := Self.Events;
   begin
      if Self.Connecting then
         Self.Connecting := False;
         Self.Error := Get_Error;

         if not Self.Error.Is_Empty then
            Disconnect (Self.Error);
         elsif Self.Listener = null then
            Self.Change_Watch ((others => False));
         else
            declare
               Remote : Network.Addresses.Address;
            begin
               Self.Is_Closed := False;
               Self.Change_Watch (not Write_Event);
               Self.Listener.Connected (Remote);
            end;
         end if;
      elsif Self.Is_Closed then
         null;
      else
         pragma Assert (Self.Listener /= null);
         Self.In_Event := True;
         Self.Events := Self.Events and not Events;

         if Events (Network.Polls.Input)
           and then not Self.Is_Closed
         then
            pragma Assert (Self.Error.Is_Empty);
            Self.Listener.Can_Read;
         end if;

         if Events (Network.Polls.Output)
           and then not Self.Is_Closed
           and then Self.Error.Is_Empty
         then
            Self.Listener.Can_Write;
         end if;

         if Self.Is_Closed then
            Disconnect (League.Strings.Empty_Universal_String);
         elsif not Self.Error.Is_Empty then
            Disconnect (Self.Error);
         elsif Events (Network.Polls.Error) then
            Disconnect (Get_Error);
         elsif Events (Network.Polls.Close) then
            Disconnect (League.Strings.Empty_Universal_String);
         elsif Self.Events /= Prev then
            Self.Change_Watch (Self.Events);
         end if;

         Self.In_Event := False;
      end if;
   end On_Event;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Out_Socket;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Kind : GNAT.Sockets.Error_Type;
   begin
      GNAT.Sockets.Receive_Socket (Self.Internal, Data, Last);

      if Last < Data'First then  --  End of stream
         if Self.In_Event then
            Self.Is_Closed := True;
         else
            Self.Change_Watch ((others => False));
            GNAT.Sockets.Close_Socket (Self.Internal);
            Self.Listener.Closed (League.Strings.Empty_Universal_String);
         end if;
      end if;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Last := Data'First - 1;
         Kind := GNAT.Sockets.Resolve_Exception (E);

         if Kind /= GNAT.Sockets.Resource_Temporarily_Unavailable then
            Self.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));

            if not Self.In_Event then
               Self.Change_Watch ((others => False));
               GNAT.Sockets.Close_Socket (Self.Internal);
               Self.Listener.Closed (Self.Error);
            end if;

         elsif Self.In_Event then
            Self.Events := Self.Events or Read_Event;

         else
            Self.Change_Watch (Self.Events or Read_Event);
         end if;
   end Read;

   --------------
   -- Register --
   --------------

   procedure Register (Manager : in out Network.Managers.Manager) is
   begin
      Manager.Register (new Protocol);
   end Register;

   ------------------
   -- Set_Listener --
   ------------------

   overriding procedure Set_Listener
     (Self     : in out Out_Socket;
      Listener : Network.Connections.Event_Listener_Access)
   is
      use type Network.Connections.Event_Listener_Access;
   begin
      pragma Assert (Self.Listener = null);
      Self.Listener := Listener;

      if not Self.Connecting then
         if Self.Error.Is_Empty then
            declare
               Remote : Network.Addresses.Address;
            begin
               Self.Is_Closed := False;
               Self.Change_Watch (not Write_Event);
               Self.Listener.Connected (Remote);
            end;
         else
            Listener.Closed (Self.Error);
         end if;
      end if;
   end Set_Listener;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Out_Socket;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Kind : GNAT.Sockets.Error_Type;
   begin
      GNAT.Sockets.Send_Socket (Self.Internal, Data, Last);

      if Last < Data'First then  --  End of stream
         if Self.In_Event then
            Self.Is_Closed := True;
         else
            Self.Change_Watch ((others => False));
            GNAT.Sockets.Close_Socket (Self.Internal);
            Self.Listener.Closed (League.Strings.Empty_Universal_String);
         end if;
      end if;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Last := Data'First - 1;
         Kind := GNAT.Sockets.Resolve_Exception (E);

         if Kind /= GNAT.Sockets.Resource_Temporarily_Unavailable then
            Self.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));

            if not Self.In_Event then
               Self.Change_Watch ((others => False));
               GNAT.Sockets.Close_Socket (Self.Internal);
               Self.Listener.Closed (Self.Error);
            end if;

         elsif Self.In_Event then
            Self.Events := Self.Events or Write_Event;

         else
            Self.Change_Watch (Self.Events or Write_Event);
         end if;
   end Write;

end Network.Managers.TCP_V4;

--  SPDX-FileCopyrightText: 2022-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Interfaces.C;

with Ada.Exceptions;

package body Network.Managers.TCP_V4_In is

   procedure Change_Watch
     (Self : in out In_Socket'Class;
      Set  : Network.Polls.Event_Set);

   use type Network.Polls.Event_Set;

   Write_Event : constant Network.Polls.Event_Set :=
     (Network.Polls.Output => True, others => False);

   Read_Event : constant Network.Polls.Event_Set :=
     (Network.Polls.Input => True, others => False);

   ------------------
   -- Change_Watch --
   ------------------

   procedure Change_Watch
     (Self : in out In_Socket'Class;
      Set  : Network.Polls.Event_Set)
   is
   begin
      Self.Manager.Poll.Change_Watch
        (Set,
         Interfaces.C.int (GNAT.Sockets.To_C (Self.Internal)),
         Self'Unchecked_Access);

      Self.Events := Set;
   end Change_Watch;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Self : in out In_Socket) is
   begin
      pragma Assert (not Self.Is_Closed);  --  Precondition
      Self.Change_Watch ((others => False));
      GNAT.Sockets.Close_Socket (Self.Internal);
      Self.Manager.Delete_Connection (Self'Unchecked_Access);
      Self.Is_Closed := True;
   end Close;

   -----------------
   -- Dereference --
   -----------------

   overriding function Dereference (Self : in out In_Socket) return Boolean is
   begin
      return Result : constant Boolean :=
        System.Atomic_Counters.Decrement (Self.Counter)
      do
         if Result and then not Self.Is_Closed then
            Self.Close;
         end if;
      end return;
   end Dereference;

   ------------------
   -- Has_Listener --
   ------------------

   overriding function Has_Listener (Self : In_Socket) return Boolean is
   begin
      return Self.Listener.Assigned;
   end Has_Listener;

   ---------------
   -- Is_Closed --
   ---------------

   overriding function Is_Closed (Self : In_Socket) return Boolean is
   begin
      return Self.Is_Closed;
   end Is_Closed;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out In_Socket;
      Events : Network.Polls.Event_Set)
   is
      use type Network.Connections.Listener_Access;

      function Get_Error return League.Strings.Universal_String;
      procedure Disconnect (Error : League.Strings.Universal_String);

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect (Error : League.Strings.Universal_String) is
      begin
         Self.Change_Watch ((others => False));
         GNAT.Sockets.Close_Socket (Self.Internal);
         Self.Manager.Delete_Connection (Self'Unchecked_Access);
         Self.Is_Closed := True;

         if Self.Listener.Assigned then
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
      --  Active poll events

      Input    : constant Network.Polls.Event := Network.Polls.Input;
      Output   : constant Network.Polls.Event := Network.Polls.Output;
      Listener : array (Input .. Output) of Network.Connections.Listener_Access
        := (others => Self.Listener);

      Again : Boolean := True;

   begin
      Self.In_Event := True;

      if Self.Is_Closed then
         --  Connection has been closed, but some events arrive after that.
         null;
      else
         pragma Assert (Self.Listener.Assigned);
         pragma Assert (Self.Error.Is_Empty);

         Self.Events := Self.Events and not Events;

         --  Report read event to current listener
         if Events (Input) then  --  Have something to read
            Self.Listener.Can_Read;
            --  This can change Self.Events, Self.Listener or close
         end if;

         --  Report write event to current listener
         if Events (Output)
           and not Self.Events (Output) --  Have space to write
           and Self.Listener = Listener (Output)
           and not Self.Is_Closed
         then
            Self.Listener.Can_Write;
            --  This can change Self.Events, Self.Listener or close
         end if;

         --  Now report to changed listener if any
         while Again loop
            Again := False;

            if not Self.Events (Input)  --  Can read
              and Self.Listener /= Listener (Input)
              and not Self.Is_Closed
            then
               Listener (Input) := Self.Listener;
               Self.Listener.Can_Read;
               Again := True;
            end if;

            if not Self.Events (Output)  --  Can write
              and Self.Listener /= Listener (Output)
              and not Self.Is_Closed
            then
               Listener (Output) := Self.Listener;
               Self.Listener.Can_Write;
               Again := True;
            end if;
         end loop;

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
      end if;

      Self.In_Event := False;
   end On_Event;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out In_Socket;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Kind : GNAT.Sockets.Error_Type;
   begin
      GNAT.Sockets.Receive_Socket (Self.Internal, Data, Last);

      if Last < Data'First then  --  End of stream
         Self.Is_Closed := True;

         if not Self.In_Event then
            raise Program_Error with "Unimplemented";
         end if;
      end if;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Last := Data'First - 1;
         Kind := GNAT.Sockets.Resolve_Exception (E);

         if Kind /= GNAT.Sockets.Resource_Temporarily_Unavailable then
            Self.Is_Closed := True;
            Self.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));

            if not Self.In_Event then
               raise Program_Error with "Unimplemented";
            end if;

         elsif Self.In_Event then
            Self.Events := Self.Events or Read_Event;

         else
            Self.Change_Watch (Self.Events or Read_Event);
         end if;
   end Read;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out In_Socket) is
   begin
      System.Atomic_Counters.Increment (Self.Counter);
   end Reference;

   ------------
   -- Remote --
   ------------

   overriding function Remote (Self : In_Socket)
     return Network.Addresses.Address is (Self.Remote);

   ------------------------
   -- Set_Input_Listener --
   ------------------------

   overriding procedure Set_Input_Listener
     (Self  : in out In_Socket;
      Value : Network.Streams.Input_Listener_Access)
   is
   begin
      Self.Listener := Network.Connections.Listener_Access (Value);

      if not Self.Error.Is_Empty then
         Self.Listener.Closed (Self.Error);
      end if;
   end Set_Input_Listener;

   -------------------------
   -- Set_Output_Listener --
   -------------------------

   overriding procedure Set_Output_Listener
     (Self  : in out In_Socket;
      Value : Network.Streams.Output_Listener_Access)
   is
      use type Network.Connections.Listener_Access;
   begin
      pragma Assert
        (Self.Listener = Network.Connections.Listener_Access (Value));
   end Set_Output_Listener;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out In_Socket;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Kind : GNAT.Sockets.Error_Type;
   begin
      GNAT.Sockets.Send_Socket (Self.Internal, Data, Last);

      if Last < Data'First then  --  End of stream
         Self.Is_Closed := True;

         if not Self.In_Event then
            raise Program_Error with "Unimplemented";
         end if;
      end if;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Last := Data'First - 1;
         Kind := GNAT.Sockets.Resolve_Exception (E);

         if Kind /= GNAT.Sockets.Resource_Temporarily_Unavailable then
            Self.Is_Closed := True;
            Self.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));

            if not Self.In_Event then
               raise Program_Error with "Unimplemented";
            end if;

         elsif Self.In_Event then
            Self.Events := Self.Events or Write_Event;

         else
            Self.Change_Watch (Self.Events or Write_Event);
         end if;
   end Write;

end Network.Managers.TCP_V4_In;

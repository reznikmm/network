--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Streams;

with GNAT.Sockets;

with Network.Streams;

private
package Network.Managers.TCP_V4_In is

   type In_Socket (Poll : Network.Polls.Poll_Access) is
     limited new Network.Polls.Listener
       and Network.Connections.Connection with
   record
      Error      : League.Strings.Universal_String;
      Internal   : GNAT.Sockets.Socket_Type;
      Events     : Network.Polls.Event_Set := (others => False);
      --  When In_Event, desired events to watch in Poll, otherwise active
      --  watching events.
      Is_Closed  : Boolean := False;  --  Has been closed already
      In_Event   : Boolean := False;  --  Inside On_Event
      Remote     : Network.Addresses.Address;
      Listener   : Network.Connections.Listener_Access;
   end record;

   type In_Socket_Access is access all In_Socket;

   overriding function Is_Closed (Self : In_Socket) return Boolean;

   overriding procedure Set_Input_Listener
     (Self  : in out In_Socket;
      Value : Network.Streams.Input_Listener_Access);

   overriding procedure Set_Output_Listener
     (Self  : in out In_Socket;
      Value : Network.Streams.Output_Listener_Access);

   overriding function Has_Listener (Self : In_Socket) return Boolean;

   overriding procedure On_Event
     (Self   : in out In_Socket;
      Events : Network.Polls.Event_Set);

   overriding procedure Read
     (Self : in out In_Socket;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out In_Socket;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Close (Self : in out In_Socket);

   overriding function Remote (Self : In_Socket)
     return Network.Addresses.Address;

end Network.Managers.TCP_V4_In;

--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Network.Addresses;
with Network.Streams;

package Network.Connections is
   pragma Preelaborate;

   type Connection is limited interface
     and Network.Streams.Input_Stream
     and Network.Streams.Output_Stream;
   --  A network connection is a bidirectional stream

   type Connection_Access is access all Connection'Class
     with Storage_Size => 0;

   not overriding function Remote (Self : Connection)
     return Network.Addresses.Address is abstract;
   --  An address of the connection remote side

   type Listener is limited interface
     and Network.Streams.Input_Listener
     and Network.Streams.Output_Listener;

   function Assigned (Self : access Listener'Class) return Boolean is
     (Self /= null);
   --  Check in Self is not null

   type Listener_Access is access all Listener'Class with Storage_Size => 0;

   procedure Set_Listener
     (Self  : in out Connection'Class;
      Value : Listener_Access);
   --  Assign a listener to the connection. Use this instead of
   --  Set_Input_Listener/Set_Output_Listener.

end Network.Connections;

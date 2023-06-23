--  SPDX-FileCopyrightText: 2021-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers;
with Ada.Finalization;
with Ada.Streams;

private with Network.Abstract_Connections;
with Network.Addresses;
with Network.Streams;

package Network.Connections is
   pragma Preelaborate;

   type Connection is new Network.Streams.Input_Stream
     and Network.Streams.Output_Stream with private;
   --  A network connection is a bidirectional stream

   not overriding function Remote (Self : Connection)
     return Network.Addresses.Address;
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

   function Hash (Self : Connection) return Ada.Containers.Hash_Type;
   --  Hash function for the connection

   function Null_Connection return Connection;

private

   type Connection_Access is access all
     Network.Abstract_Connections.Abstract_Connection'Class;

   type Connection is new Ada.Finalization.Controlled
     and Network.Streams.Input_Stream
     and Network.Streams.Output_Stream with
   record
      Object : Connection_Access;
   end record;

   --  overriding procedure Initialize (Self : in out Connection);
   overriding procedure Adjust (Self : in out Connection);
   overriding procedure Finalize (Self : in out Connection);

   overriding function Has_Listener (Self : Connection) return Boolean is
      (Self.Object /= null and then Self.Object.Has_Listener);

   overriding function Is_Closed (Self : Connection) return Boolean is
      (Self.Object = null or else Self.Object.Is_Closed);

   overriding procedure Set_Input_Listener
     (Self     : in out Connection;
      Listener : Network.Streams.Input_Listener_Access);

   overriding procedure Read
     (Self : in out Connection;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Set_Output_Listener
     (Self     : in out Connection;
      Listener : Network.Streams.Output_Listener_Access);

   overriding procedure Write
     (Self : in out Connection;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Close (Self : in out Connection);

   function Null_Connection return Connection is
      (Ada.Finalization.Controlled with Object => null);

end Network.Connections;

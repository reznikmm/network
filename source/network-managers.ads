--  SPDX-FileCopyrightText: 2021-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers.Vectors;

with League.String_Vectors;
with League.Strings;

with Network.Abstract_Connections;
with Network.Addresses;
with Network.Connections;
with Network.Polls;

package Network.Managers is
   pragma Preelaborate;

   type Manager is tagged limited private;
   --  Network connection manager

   type Connection_Listener is limited interface;
   --  The interface for receiving connection establishment/acceptance
   --  notifications.

   type Connection_Listener_Access is access all Connection_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Connected
     (Self       : in out Connection_Listener;
      Connection : in out Network.Connections.Connection;
      Remote     : Network.Addresses.Address) is abstract
     with Post'Class => Connection.Has_Listener;
   --  This procedure is called when the connection manager has accepted an
   --  incoming connection or established an outgoing connection.

   not overriding procedure Rejected
     (Self   : in out Connection_Listener;
      Error  : League.Strings.Universal_String;
      Remote : Network.Addresses.Address) is null;
   --  This procedure is called when the connection manager has failed to
   --  establish an outgoing connection.

   procedure Initialize (Self : in out Manager'Class);
   --  Call Initialize before use

   procedure Connect
     (Self     : in out Manager'Class;
      Address  : Network.Addresses.Address;
      Error    : out League.Strings.Universal_String;
      Listener : not null Connection_Listener_Access;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);
   --  Try to connect to given address asynchronously. Notify the listener
   --  when connection established or rejected. Once notified, the listener is
   --  no longer in use and can be deleted. Options are protocol dependent.

   procedure Listen
     (Self     : in out Manager'Class;
      List     : Network.Addresses.Address_Array;
      Listener : not null Connection_Listener_Access;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);
   --  Make manager to listen given network addresses and call listener on
   --  new incoming connection. Return Error if some address isn't supported.
   --  Options are protocol dependent.

   procedure Wait
     (Self    : in out Manager'Class;
      Timeout : Duration);
   --  Run manager for specified time or till some event occurs.

private

   type Connection_Access is access all
     Network.Abstract_Connections.Abstract_Connection'Class;
   type Protocol is limited interface;
   type Protocol_Access is access all Protocol'Class with Storage_Size => 0;

   not overriding function Can_Listen
     (Self    : Protocol;
      Address : Network.Addresses.Address) return Boolean is abstract;

   not overriding function Can_Connect
     (Self    : Protocol;
      Address : Network.Addresses.Address) return Boolean is abstract;

   not overriding procedure Listen
     (Self     : in out Protocol;
      List     : Network.Addresses.Address_Array;
      Listener : not null Connection_Listener_Access;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector) is abstract;

   not overriding procedure Connect
     (Self     : in out Protocol;
      Address  : Network.Addresses.Address;
      Error    : out League.Strings.Universal_String;
      Listener : not null Connection_Listener_Access;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector) is abstract;

   procedure Register
     (Self     : in out Manager;
      Protocol : not null Protocol_Access);

   type Protocol_Access_Array is array (Positive range <>) of Protocol_Access;

   package Connection_Vectors is new Ada.Containers.Vectors
     (Positive, Connection_Access);

   type Manager is tagged limited record
      Poll    : Network.Polls.Poll;
      Proto   : Protocol_Access_Array (1 .. 10);
      Last    : Natural := 0;
      Deleted : Connection_Vectors.Vector;
   end record;

   procedure New_Connection
     (Self       : in out Manager;
      Connection : not null access
        Network.Abstract_Connections.Abstract_Connection'Class);

   procedure Delete_Connection
     (Self       : in out Manager;
      Connection : not null access
        Network.Abstract_Connections.Abstract_Connection'Class);

   type Manager_Access is access all Network.Managers.Manager;

end Network.Managers;

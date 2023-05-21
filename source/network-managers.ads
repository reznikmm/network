--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with League.String_Vectors;
with League.Strings;

with Network.Addresses;
with Network.Connections;
with Network.Connection_Promises;
with Network.Polls;

package Network.Managers is
   pragma Preelaborate;

   type Manager is tagged limited private;
   --  Network connection manager

   procedure Initialize (Self : in out Manager'Class);
   --  Call Initialize before use

   procedure Connect
     (Self    : in out Manager'Class;
      Address : Network.Addresses.Address;
      Error   : out League.Strings.Universal_String;
      Promise : out Connection_Promises.Promise;
      Options : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);
   --  Try to connect to given address asynchronously. Return a connection
   --  promise or an Error if the address isn't supported. The promise will
   --  be resolved with a connection or rejected with an error. On successful
   --  connect the application should set a listener to the connection. After
   --  that it could write and read data until get a close event. Options are
   --  protocol dependent.

   type Connection_Listener is limited interface;
   type Connection_Listener_Access is access all Connection_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Connected
     (Self       : in out Connection_Listener;
      Connection : in out Network.Connections.Connection;
      Remote     : Network.Addresses.Address) is abstract
     with Post'Class => Connection.Has_Listener;
   --  Once the manager accepts a new connection. It should assign a listener
   --  to the connection.

   procedure Listen
     (Self     : in out Manager'Class;
      List     : Network.Addresses.Address_Array;
      Listener : Connection_Listener_Access;
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
      Listener : Connection_Listener_Access;
      Poll     : in out Network.Polls.Poll;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector) is abstract;

   not overriding procedure Connect
     (Self    : in out Protocol;
      Address : Network.Addresses.Address;
      Poll    : in out Network.Polls.Poll;
      Error   : out League.Strings.Universal_String;
      Promise : out Connection_Promises.Promise;
      Options : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector) is abstract;

   procedure Register
     (Self     : in out Manager;
      Protocol : not null Protocol_Access);

   type Protocol_Access_Array is array (Positive range <>) of Protocol_Access;

   type Manager is tagged limited record
      Poll  : Network.Polls.Poll;
      Proto : Protocol_Access_Array (1 .. 10);
      Last  : Natural := 0;
   end record;

end Network.Managers;

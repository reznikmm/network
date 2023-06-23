--  SPDX-FileCopyrightText: 2021-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with GNAT.Sockets;

private with Ada.Containers.Vectors;
private with Network.Managers.TCP_V4_Listen;

package Network.Managers.TCP_V4 is

   procedure Register (Manager : in out Network.Managers.Manager);

   function Remote (Value : GNAT.Sockets.Sock_Addr_Type)
     return Network.Addresses.Address;

private

   type Listen_Socket_Access is access all TCP_V4_Listen.Listen_Socket;

   package Listen_Socket_Vectors is new Ada.Containers.Vectors
     (Positive, Listen_Socket_Access);

   type Protocol (Manager : not null Manager_Access) is
     new Network.Managers.Protocol with
   record
      Listen  : Listen_Socket_Vectors.Vector;
   end record;

   overriding function Can_Listen
     (Self    : Protocol;
      Address : Network.Addresses.Address) return Boolean;

   overriding function Can_Connect
     (Self    : Protocol;
      Address : Network.Addresses.Address) return Boolean;

   overriding procedure Listen
     (Self     : in out Protocol;
      List     : Network.Addresses.Address_Array;
      Listener : not null Connection_Listener_Access;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);

   overriding procedure Connect
     (Self     : in out Protocol;
      Address  : Network.Addresses.Address;
      Error    : out League.Strings.Universal_String;
      Listener : not null Connection_Listener_Access;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);

end Network.Managers.TCP_V4;

--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Network.Managers.TCP_V4 is

   procedure Register (Manager : in out Network.Managers.Manager);

private

   type Protocol is new Network.Managers.Protocol with record
      null;
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
      Listener : Connection_Listener_Access;
      Poll     : in out Network.Polls.Poll;
      Error    : out League.Strings.Universal_String;
      Options  : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);

   overriding procedure Connect
     (Self    : in out Protocol;
      Address : Network.Addresses.Address;
      Poll    : in out Network.Polls.Poll;
      Error   : out League.Strings.Universal_String;
      Promise : out Network.Connection_Promises.Promise;
      Options : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector);

end Network.Managers.TCP_V4;

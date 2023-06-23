--  SPDX-FileCopyrightText: 2022-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with GNAT.Sockets;

private
package Network.Managers.TCP_V4_Listen is

   type Listen_Socket (Manager : not null Manager_Access) is
     limited new Network.Polls.Listener with
   record
      Error      : League.Strings.Universal_String;
      Internal   : GNAT.Sockets.Socket_Type;
      Events     : Network.Polls.Event_Set := (others => False);
      Listener   : Network.Managers.Connection_Listener_Access;
   end record;

   overriding procedure On_Event
     (Self   : in out Listen_Socket;
      Events : Network.Polls.Event_Set);

end Network.Managers.TCP_V4_Listen;

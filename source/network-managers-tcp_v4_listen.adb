--  SPDX-FileCopyrightText: 2022-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Exceptions;
with Interfaces.C;

with Network.Connections.Internal;
with Network.Managers.TCP_V4;
with Network.Managers.TCP_V4_In;

package body Network.Managers.TCP_V4_Listen is

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Listen_Socket;
      Events : Network.Polls.Event_Set)
   is
      Remote : GNAT.Sockets.Sock_Addr_Type;
      Req    : GNAT.Sockets.Request_Type :=
        (GNAT.Sockets.Non_Blocking_IO, Enabled => True);
      Socket : constant TCP_V4_In.In_Socket_Access :=
        new TCP_V4_In.In_Socket (Self.Manager);
   begin
      begin
         GNAT.Sockets.Accept_Socket (Self.Internal, Socket.Internal, Remote);
      exception
         when E : GNAT.Sockets.Socket_Error =>
            Socket.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));
            --  then what???
      end;

      Self.Manager.New_Connection (Socket);
      Socket.Remote := TCP_V4.Remote (Remote);
      GNAT.Sockets.Control_Socket (Socket.Internal, Req);

      declare
         Connection : Network.Connections.Connection :=
           Network.Connections.Internal.Cast (Socket);
         Ignore : Boolean;
      begin
         Self.Listener.Connected
           (Connection => Connection,
            Remote     => Socket.Remote);

         pragma Assert (Socket.Listener.Assigned);
         Socket.Events := (others => True);

         Self.Manager.Poll.Watch
           (Interfaces.C.int (GNAT.Sockets.To_C (Socket.Internal)),
            Events   => Socket.Events,
            Listener => Socket.all'Access);

         Ignore := Socket.Dereference;
      end;
   end On_Event;

end Network.Managers.TCP_V4_Listen;

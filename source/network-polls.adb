--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Network.Polls is

   use type Interfaces.C.int;
   use type Interfaces.Unsigned_32;

   EPOLL_CLOEXEC : constant := 8#02000000#;

   pragma Warnings (Off);
   EPOLLIN     : constant := 16#001#;
   EPOLLPRI    : constant := 16#002#;
   EPOLLOUT    : constant := 16#004#;
   EPOLLRDNORM : constant := 16#040#;
   EPOLLRDBAND : constant := 16#080#;
   EPOLLWRNORM : constant := 16#100#;
   EPOLLWRBAND : constant := 16#200#;
   EPOLLMSG    : constant := 16#400#;
   EPOLLERR    : constant := 16#008#;
   EPOLLHUP    : constant := 16#010#;
   EPOLLRDHUP  : constant := 16#2000#;
   --     EPOLLEXCLUSIVE = 1u << 28,
   EPOLLEXCLUSIVE : constant := 16#1000_0000#;
   --     EPOLLWAKEUP = 1u << 29,
   EPOLLWAKEUP    : constant := 16#2000_0000#;
   --     EPOLLONESHOT = 1u << 30,
   EPOLLONESHOT   : constant := 16#4000_0000#;
   --     EPOLLET = 1u << 31
   EPOLLET        : constant := 16#8000_0000#;

   EPOLL_CTL_ADD : constant := 1;
   EPOLL_CTL_DEL : constant := 2;
   EPOLL_CTL_MOD : constant := 3;
   pragma Warnings (On);

   function epoll_create1
     (Flag : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_create1";

   type epoll_event is record
      events : Interfaces.Unsigned_32;
      data   : Listener_Access;
   end record
     with Convention => C, Pack;

   function epoll_ctl
     (epfd  : Interfaces.C.int;
      op    : Interfaces.C.int;
      fd    : Interfaces.C.int;
      event : epoll_event) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_ctl";

   type epoll_event_array is array (Positive range <>) of epoll_event;

   function epoll_wait
     (epfd      : Interfaces.C.int;
      events    : out epoll_event_array;
      maxevents : Interfaces.C.int := 1;
      timeout   : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_wait";

   Map : constant array (Event) of Interfaces.Unsigned_32 :=
     (Input   => EPOLLIN,
      Output  => EPOLLOUT,
      Error   => EPOLLERR,
      Close   => EPOLLHUP);

   function To_Int (Set : Event_Set) return Interfaces.Unsigned_32;

   ------------------
   -- Change_Watch --
   ------------------

   procedure Change_Watch
     (Self     : in out Poll'Class;
      Events   : Event_Set;
      Value    : FD;
      Listener : Listener_Access)
   is
      Op : constant Interfaces.C.int :=
        (if Events = (Event_Set'Range => False) then EPOLL_CTL_DEL
         else EPOLL_CTL_MOD);

      Data   : constant epoll_event := (To_Int (Events), Listener);
      Result : constant Interfaces.C.int := epoll_ctl
        (Self.Internal, Op, Value, Data);
   begin
      pragma Assert (Result = 0);

      if Op = EPOLL_CTL_DEL then
         Self.Count := Self.Count - 1;
      end if;
   end Change_Watch;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Poll) is
      procedure close (Value : FD)
        with Import, Convention => C, External_Name => "close";
   begin
      if Self.Is_Initialized then
         close (Self.Internal);
         Self.Internal := 0;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : out Poll'Class) is
   begin
      Self.Count := 0;
      Self.Internal := epoll_create1 (EPOLL_CLOEXEC);
   end Initialize;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized (Self : Poll'Class) return Boolean is
   begin
      return Self.Internal /= 0;
   end Is_Initialized;

   ------------
   -- To_Int --
   ------------

   function To_Int (Set : Event_Set) return Interfaces.Unsigned_32 is
      All_Events : Interfaces.Unsigned_32 := 0;
   begin
      for J in Set'Range loop
         if Set (J) then
            All_Events := All_Events + Map (J);
         end if;
      end loop;

      return All_Events;
   end To_Int;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Self    : in out Poll'Class;
      Timeout : Duration)
   is
      function To_Set (X : Interfaces.Unsigned_32) return Event_Set;

      ------------
      -- To_Set --
      ------------

      function To_Set (X : Interfaces.Unsigned_32) return Event_Set is
         Result : Event_Set;
      begin
         for J in Result'Range loop
            Result (J) := (X and Map (J)) /= 0;
         end loop;

         return Result;
      end To_Set;

      Data : epoll_event_array (1 .. Self.Count);

   begin
      if Self.Count = 0 then
         delay Timeout;
         return;
      end if;

      declare
         Result : constant Interfaces.C.int := epoll_wait
           (epfd      => Self.Internal,
            events    => Data,
            maxevents => Data'Length,
            timeout   => Interfaces.C.int (1000 * Timeout));
      begin
         pragma Assert (Result >= 0);

         for X of Data (1 .. Natural (Result)) loop
            X.data.On_Event (To_Set (X.events));
         end loop;
      end;
   end Wait;

   -----------
   -- Watch --
   -----------

   procedure Watch
     (Self     : in out Poll'Class;
      Value    : FD;
      Events   : Event_Set;
      Listener : Listener_Access)
   is
      Data   : constant epoll_event := (To_Int (Events), Listener);
      Result : constant Interfaces.C.int := epoll_ctl
        (Self.Internal, EPOLL_CTL_ADD, Value, Data);
   begin
      pragma Assert (Result = 0);
      Self.Count := Self.Count + 1;
   end Watch;

end Network.Polls;

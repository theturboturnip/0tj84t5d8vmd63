# different approach: use 


from dataclasses import dataclass
import random
from typing import List, Optional, TypeAlias

Time: TypeAlias = int

@dataclass
class WorkItem:
    key_id: int
    mapped_tick: Time
    # TODO could put a dependency on other items
    completed_tick: Time
    unmapped_req_tick: Optional[Time]
    unmapped_at_tick: Time

@dataclass
class KeySuballocatorState:
    active_mappings: int
    num_mappings_left: int
    quarantine: List[WorkItem]

class KeySuballocator:
    per_epoch_max_references: int
    key_stats: List[KeySuballocatorState]
    insertion_key: int
    is_full: bool

    def __init__(self, n_epochs: int, per_epoch_max_references: int,):
        self.per_epoch_max_references = per_epoch_max_references
        self.key_stats = []
        for i in range(n_epochs):
            self.key_stats.append(KeySuballocatorState(
                active_mappings=0,
                num_mappings_left=per_epoch_max_references,
                quarantine=[]
            ))
        self.insertion_key = 0
        self.is_full = False

    def map_item(self) -> Optional[int]:
        if self.is_full:
            return None
        
        state = self.key_stats[self.insertion_key]

        state.active_mappings += 1
        if state.active_mappings == 1:
            pass # activating key

        state.num_mappings_left -= 1
        assert state.num_mappings_left >= 0

        chosen_epoch = self.insertion_key
        if state.num_mappings_left == 0:
            found_next_key = False
            for i in range(1, len(self.key_active_mappings)):
                next_key = (chosen_epoch + i) % len(self.key_active_mappings)
                if self.key_stats[next_key].num_mappings_left > 0:
                    found_next_key = True
                    break
            if found_next_key:
                self.insertion_key = next_key
            else:
                self.is_full = True
        return chosen_epoch
        
    def unmap_item(self, sim: "Sim", item: WorkItem):
        assert item.unmapped_req_tick is None
        item.unmapped_req_tick = sim.cur_time

        state = self.key_stats[item.key_id]

        state.active_mappings -= 1
        assert state.active_mappings >= 0
        
        if state.active_mappings == 0:
            # deactivating key
            # clear the key
            # flush quarantine
            for item in state.quarantine:
                item.unmapped_at_tick = sim.cur_time
            state.quarantine = []
            # call this quarantnme callback
            item.unmapped_at_tick = sim.cur_time
            # if is_full, we are full no longer
            if self.is_full:
                self.insertion_key = item.key_id
                self.is_full = False
        else:
            state.quarantine.append(item)

# Model a (in flow rate, out flow rate, out latency) device
# i.e. work comes in at one rate, takes some latency to process *and* each work item
# if out rate == in rate, the length of time to process any given work item is independent of how many other work items there are.
# if out rate > in rate, the length of time to process a work item is also independent.
    # overall the rate is limited to the in rate - it isn't suddenly faster to complete work items
# if out rate < in rate, the length of time to process a work item gets longer the more other work items there are ahead of it.
    # - you have to wait for previous work items to complete, for resources to get filled up, etc.
    # - this pre-work thingy is independent of the actual completion time - consider a device that *completes* tasks quickly by copying them into scratch memory
    # , but internally copies the scratch out to a slow ethernet. the time to return a task to the host (hey, i've used the data, you can get rid of it now) 
    #   is different to the time between tasks (sorry, can't take a task, haven't pulled anything out of the scratch memory yet)
# effectively there are two latency sources: processing and delay.
# work item latency = processing + delay * num-works-ahead.
# if out rate >= in rate, delay = 0.
# if the device can complete one task every 100 ticks, in a full queue with no incoming data the time between completing consecutive work items is 100.
# if the device gets a new work item at t=0, then completes it at t=proc
# if the devices gets another work item at t=10, then completes it at t=proc+10 (total latency = proc+10-10 = proc => delay = 0)
# if the devices gets another work item at t=10, then completes it at t=proc+10 (total latency = proc+10-10 = proc => delay = 0)
# and the host puts in one task every 100 ticks 
# if out rate < in rate, delay > 0 (different between different devices)
@dataclass
class WorkItemGenerator:

    generation_rate
    completion_rate_over_generation_rate: float
    mean_completion_ticks: int = 10000
    std_dev_completion_ticks: int = 1000

    next_tick_generating_item: Time = 0

    def choose_completion_delay(self, sim: "Sim") -> Time:
        time = -1
        while time <= 0:
            time = Time(sim.rand.normalvariate(self.mean_completion_ticks, self.std_dev_completion_ticks))
        return time
            

class Sim:
    cur_time: Time
    rand: random.Random
    gen: WorkItemGenerator
    items: List[WorkItem]
    queue: List[int] # indexes into .items
    key_suballoc: KeySuballocator

    def __init__(self, seed: int, n_epochs: int, per_epoch_max_references: int, gen: WorkItemGenerator):
        self.cur_time = 0
        self.rand = random.Random(seed)
        self.gen = gen
        self.items = []
        self.queue = []
        self.key_suballoc = KeySuballocator(n_epochs, per_epoch_max_references)


"""
first step: notice that we *need* epochs or limits to prevent infinite exposure slack.

consider a saturated queue with more than one element, where each time an event in the queue is completed a new one immediately takes its place. this is the ideal state of a high-performance system, because to leave the queue empty would potentially represent wasted work. 

assume the initial revocation strategy is to provide a single key per queue of length q. the key starts as *cleared*, becomes *issued* once an iocap is issued, stays in the *issued* state without modification each time a new iocap is issued while in that state, and moving from *issued* back to *cleared* once all iocaps issued from it have been completed.

with this initial strategy, under saturated q>1 conditions, the data under the first iocap would be exposed for infinite time because there would never be a point in time after that where all iocaps have been completed. there is always a new iocap waiting to be issued.

**techincally under random ordering conditions it can be possible to eliminate infinite slack using a group of k keys if 2(q - k) < k.**

this works because the chance of a randomly selected element from a queue (where each element picks its key from a uniform random distrubution) being the only element associated with that queue is roughly 1 - (2(q-k)/q)… i think.

this requires random selection of a key and random selection from the queue

consider a queue with elements 1 2 3 4 1. q = 5, k = 4, chance is 3/5 = 1 - (2(5-4)/5)

because the chance is >1/2, more often than not you will pick an element that is the only element associated with a key and so free it. 

1 2 3 4 1 2, q = 6, k = 4, chance is 2/6 = 1 - (2(6-4)/6)

because the chance is <1/2, more often than not you will pick an element that isn’t the only element associated with a key

1 - (2(q-k)/q) = 1 - (2q/q - 2k/q) = 1 - 2 + 2k/q = 2k/q - 1

2(q-k) < k ⇒ 2q - 2k < k ⇒ 2q < 3k
"""

import abc
from collections import defaultdict
from dataclasses import dataclass
import math
import random
from statistics import mean, median, quantiles
from typing import List, Dict, Optional, cast
from typing_extensions import override

@dataclass
class Element:
    key: int

    tick_created: int
    tick_consumed: Optional[int] = None

@dataclass
class KeyLifetime:
    tick_issued: int
    # List of elements created with this key
    elements: List[Element]

    def clear_key(self, cur_tick: int, cutting_short: bool=False) -> "KeyLifetimeStats":
        consumed_elems = [
            elem
            for elem in self.elements
            if elem.tick_consumed is not None
        ]
        if not cutting_short:
            assert len(consumed_elems) == len(self.elements)
    
        return KeyLifetimeStats(
            lifetime=cur_tick - self.tick_issued,
            num_elements=len(self.elements),
            num_unconsumed_elements=len(self.elements) - len(consumed_elems),
            num_consumed_unfreed_elements=len(self.elements) if cutting_short else 0,
            create_to_consume_times=[
                cast(int, elem.tick_consumed) - elem.tick_created
                for elem in consumed_elems
            ],
            consumed_to_freed_times=[
                cur_tick - cast(int, elem.tick_consumed)
                for elem in consumed_elems
                if not cutting_short
            ],
            max_num_element_exposure_slack=len(self.elements)
        )

@dataclass
class KeyLifetimeStats:
    lifetime: int
    num_elements: int
    num_unconsumed_elements: int
    # The elements that were left at the end of the sim, when this KeyLifetime was cut short.
    # Either 0 or equal to num_elements
    num_consumed_unfreed_elements: int
    # For each element, the amount of ticks it waited in the queue before being consumed. Will have data
    create_to_consume_times: List[int]
    # For each element, the amount of ticks it waited after being consumed before its key was cleared.
    # Empty if this run was cut short i.e. if num_consumed_unfreed_elements != 0
    consumed_to_freed_times: List[int]
    max_num_element_exposure_slack: int
    

class KeyAllocator:
    num_keys: int

    def __init__(self, num_keys: int):
        assert num_keys > 0
        self.num_keys = num_keys

    @abc.abstractmethod
    def select_key_for_new_element(self) -> Optional[int]:
        ...

    def on_key_unused(self, key: int) -> None:
        pass

class RoundRobinUnlimitedKeyAllocator(KeyAllocator):
    """
    Each tick, select the next key round-robin.
    Keys have no allocation limit.
    """
    next_key: int

    def __init__(self, num_keys):
        super().__init__(num_keys)
        self.next_key = 0

    @override
    def select_key_for_new_element(self) -> int:
        key = self.next_key
        self.next_key = (self.next_key + 1) % self.num_keys
        return key
    
class RoundRobinLimitKeyAllocator(KeyAllocator):
    """
    Select one key until it hits its allocation limit, then move on to the next.
    Keys have an allocation limit.
    If we try to allocate from a key at its allocation limit only then do we move on to the next key.
    If all keys are at the limit, don't let new elements into the queue.
    The number of allocs for each key monotonically increases until the allocator is notified that the lifetime has expired.
    """
    next_key: int
    key_limit: int
    key_allocs: Dict[int, int]

    def __init__(self, num_keys, key_limit: int):
        super().__init__(num_keys)
        self.next_key = 0
        self.key_allocs = {
            key: 0
            for key in range(num_keys)
        }
        self.key_limit = key_limit
        assert self.key_limit > 0

    @override
    def select_key_for_new_element(self) -> Optional[int]:
        if all(
            self.key_allocs[key] == self.key_limit
            for key in range(self.num_keys)
        ):
            return None

        # print(self.key_allocs, flush=True)
        while self.key_allocs[self.next_key] == self.key_limit:
            self.next_key = (self.next_key + 1) % self.num_keys

        key = self.next_key
        self.key_allocs[key] += 1
        assert self.key_allocs[key] <= self.key_limit
        return key
    
    @override
    def on_key_unused(self, key) -> None:
        self.key_allocs[key] = 0

class UniformRandomUnlimitedKeyAllocator(KeyAllocator):
    """
    Select a random key each tick with no bias.
    Keys have no allocation limit.
    """
    rand: random.Random

    def __init__(self, num_keys, rand: random.Random):
        super().__init__(num_keys)
        self.rand = rand

    @override
    def select_key_for_new_element(self):
        return self.rand.randrange(start=0, stop=self.num_keys)

class ElementConsumer:
    num_elems: int

    def __init__(self, num_elems: int):
        self.num_elems = num_elems
        pass

    @abc.abstractmethod
    def select_element_to_consume(self, curr_num_elems: int) -> int:
        ...

class FifoElementConsumer(ElementConsumer):
    """
    FIFO behaviour i.e. select the first element of the queue every time
    """
    @override
    def select_element_to_consume(self, curr_num_elems: int) -> int:
        return 0
    
class UniformRandomElementConsumer(ElementConsumer):
    """
    Select a random element from the queue with no bias
    """
    rand: random.Random

    def __init__(self, num_elems, rand: random.Random):
        super().__init__(num_elems)
        self.rand = rand

    @override
    def select_element_to_consume(self, curr_num_elems: int):
        return self.rand.randrange(start=0, stop=curr_num_elems)
    
class FrontRestrictedUniformRandomElementConsumer(ElementConsumer):
    """
    Select a random element from the first 'num_front_elems' elements of the queue,
    which could be a more accurate model of real out-of-order behaviour in some cases e.g. NICs?
    """
    rand: random.Random
    num_front_elems: int

    def __init__(self, num_elems, rand: random.Random, num_front_elems: int):
        super().__init__(num_elems)
        self.rand = rand
        self.num_front_elems = num_front_elems
        assert num_front_elems < num_elems

    @override
    def select_element_to_consume(self, curr_num_elems: int):
        return self.rand.randrange(start=0, stop=min(self.num_front_elems, curr_num_elems))

@dataclass
class SaturatedQueueStats:
    n_ticks: int
    per_key_lifetime_stats: Dict[int, List[KeyLifetimeStats]]
    key_alloc_backpressure_ticks: int
    consumed_but_not_freed: List[int]

class SaturatedQueue:
    key_alloc: KeyAllocator
    elem_consume: ElementConsumer

    def __init__(self, key_alloc: KeyAllocator, elem_consume: ElementConsumer):
        self.key_alloc = key_alloc
        self.elem_consume = elem_consume

    def run(self, n_ticks: int) -> SaturatedQueueStats:
        elements: List[Element] = []
        current_key_lifetimes: Dict[int, Optional[KeyLifetime]] = {
            key: None
            for key in range(self.key_alloc.num_keys)
        }
        overall_key_lifetimes: Dict[int, List[KeyLifetimeStats]] = {
            key: []
            for key in range(self.key_alloc.num_keys)
        }
        key_alloc_backpressure_ticks = 0

        for _ in range(self.elem_consume.num_elems):
            key = self.key_alloc.select_key_for_new_element()
            if key is None:
                raise RuntimeError("Not enough space in key generator to fill the queue!")
            elem = Element(
                key=key,
                tick_created=0,
            )
            elements.append(elem)
            key_lifetime = current_key_lifetimes[key]
            if key_lifetime is None:
                current_key_lifetimes[key] = KeyLifetime(
                    tick_issued=0,
                    elements=[elem]
                )
            else:
                key_lifetime.elements.append(elem)

        consumed_but_not_freed_elements = 0
        # mapping of (consumed_but_not_freed_elements value, number of ticks we had that value)
        # consumed_but_not_freed_hgram: Dict[int, int] = defaultdict(lambda: 0)
        consumed_but_not_freed = []
        key_refs = {
            key: sum(
                1
                for elem in elements
                if elem.key == key
            )
            for key in range(self.key_alloc.num_keys)
        }
        for cur_tick in range(1, n_ticks+1):
            # Consume exactly one element
            removed_elem = elements.pop(self.elem_consume.select_element_to_consume(len(elements)))
            removed_elem.tick_consumed = cur_tick
            consumed_but_not_freed_elements += 1
            key_refs[removed_elem.key] -= 1

            # Track the keys which have become unused
            unused_keys = {
                key
                for key, value in key_refs.items()
                if value == 0
            }
            for unused_key in unused_keys:
                key_lifetime = current_key_lifetimes[unused_key]
                if key_lifetime is not None:
                    # This key was deleted during this tick and is now gone from the list.
                    # Record the lifetime as the past and reset it.
                    consumed_but_not_freed_elements -= len(key_lifetime.elements)
                    overall_key_lifetimes[unused_key].append(key_lifetime.clear_key(cur_tick))
                    self.key_alloc.on_key_unused(unused_key)
                    current_key_lifetimes[unused_key] = None

            # Try to get enough new elements to saturate the queue.
            # This is usually one element, but if the key allocator has no keys available it can return None
            # and create a hole.
            # If it keeps returning None, the number of elements in the queue will decrease.
            # Once it starts returning non-None, we try to fill the queue back up as much as possible.
            has_added_elem = False
            while len(elements) < self.elem_consume.num_elems:
                new_elem_key = self.key_alloc.select_key_for_new_element()
                if new_elem_key is not None:
                    has_added_elem = True
                    elem = Element(
                        key=new_elem_key,
                        tick_created=cur_tick,
                    )
                    key_refs[new_elem_key] += 1
                    elements.append(elem)
                    key_lifetime = current_key_lifetimes[new_elem_key]
                    if key_lifetime is None:
                        current_key_lifetimes[new_elem_key] = KeyLifetime(
                            tick_issued=cur_tick,
                            elements=[elem]
                        )
                    else:
                        key_lifetime.elements.append(elem)
                else:
                    break
                
            assert len(elements) <= self.elem_consume.num_elems
            if not has_added_elem:
                key_alloc_backpressure_ticks += 1

            # consumed_but_not_freed_hgram[consumed_but_not_freed_elements] += 1
            consumed_but_not_freed.append(consumed_but_not_freed_elements)

        # Return a list of overall lifetimes
        for key in range(self.key_alloc.num_keys):
            key_lifetime = current_key_lifetimes[key]
            if key_lifetime is not None:
                # this key is still in use, record the lifetime as if it just finished
                # Record the lifetime as the past and reset it
                overall_key_lifetimes[key].append(key_lifetime.clear_key(cur_tick, cutting_short=True))
                current_key_lifetimes[key] = None
            else:
                assert key_refs[key] == 0

        assert all(life is None for life in current_key_lifetimes.values())
        return SaturatedQueueStats(
            n_ticks=n_ticks,
            per_key_lifetime_stats=overall_key_lifetimes,
            key_alloc_backpressure_ticks=key_alloc_backpressure_ticks,
            consumed_but_not_freed=consumed_but_not_freed,
        )

# def round_robin_unlimited_key_fifo_queue(num_elems: int, num_keys: int) -> SaturatedQueue:
#     return SaturatedQueue(
#         key_alloc=RoundRobinUnlimitedKeyAllocator(num_keys=num_keys),
#         elem_consume=FifoElementConsumer(num_elems=num_elems),
#     )

# def round_robin_limited_key_fifo_queue(num_elems: int, num_keys: int, key_lk) -> SaturatedQueue:
#     return SaturatedQueue(
#         key_alloc=RoundRobinLimitKeyAllocator(num_keys=num_keys, key_limit=key_limit),
#         elem_consume=FifoElementConsumer(num_elems=num_elems),
#     )

def permissive_mean(xs):
    return mean(xs) if xs else None

def permissive_median(xs):
    return median(xs) if xs else None

def permissive_quantiles(xs):
    return (quantiles(xs, n=10, method='inclusive')) if xs else None

# https://stackoverflow.com/a/31631711
def humanbytes(B):
    """Return the given bytes as a human friendly KB, MB, GB, or TB string."""
    B = float(B)
    KB = float(1024)
    MB = float(KB ** 2) # 1,048,576
    GB = float(KB ** 3) # 1,073,741,824
    TB = float(KB ** 4) # 1,099,511,627,776

    if B < KB:
        return '{0} {1}'.format(B,'Bytes' if 0 == B > 1 else 'Byte')
    elif KB <= B < MB:
        return '{0:.2f} KB'.format(B / KB)
    elif MB <= B < GB:
        return '{0:.2f} MB'.format(B / MB)
    elif GB <= B < TB:
        return '{0:.2f} GB'.format(B / GB)
    elif TB <= B:
        return '{0:.2f} TB'.format(B / TB)

def log_stats(stats: SaturatedQueueStats, detail=False, elem_size: int=1, n_queues: int=1):
    print("-------")
    print(
        f"Runtime\t{stats.n_ticks}"
    )
    if detail:
        for key, key_stats in stats.per_key_lifetime_stats.items():
            all_lifetimes = [s.lifetime for s in key_stats]
            all_create_to_consume_times = sum((s.create_to_consume_times for s in key_stats), start=[])
            all_consume_to_free_times = sum((s.consumed_to_freed_times for s in key_stats), start=[])
            print(
                f"Key{key}NumStats\t{len(key_stats)}\n"
                f"Key{key}TotalElems\t{sum(s.num_elements for s in key_stats)}\n"
                f"Key{key}TotalUnconsumedElems\t{sum(s.num_unconsumed_elements for s in key_stats)}\n"
                f"Key{key}TotalUnfreedElems\t{sum(s.num_consumed_unfreed_elements for s in key_stats)}\n"
                f"\tMean\tMedian\n"
                f"Key{key}Lifetime\t{permissive_mean(all_lifetimes)}\t{permissive_median(all_lifetimes)}\n"
                f"\tMean\tQuantiles\n"
                f"Key{key}CreateToConsume\t{permissive_mean(all_create_to_consume_times)}\t{permissive_quantiles(all_create_to_consume_times)}\n"
                f"Key{key}ConsumeToFree\t{permissive_mean(all_consume_to_free_times)}\t{permissive_quantiles(all_consume_to_free_times)}\n"
            )
    print(f"TicksBlocked\t{stats.key_alloc_backpressure_ticks}")
    print(f"MaxConsumedNotFreed\t{max(stats.consumed_but_not_freed)}")
    if elem_size != 1:
        print(f"MaxConsumedNotFreedBPerQ\t{humanbytes(max(stats.consumed_but_not_freed) * elem_size)}")
    if n_queues != 1:
        print(f"NQueues                 \t{n_queues}")
        print(f"MaxConsumedNotFreedTotalB\t{humanbytes(max(stats.consumed_but_not_freed) * elem_size * n_queues)}")
    print(f"QuartConsumedNotFreed\t{permissive_quantiles(stats.consumed_but_not_freed)}")
    print("-------")

if __name__ == '__main__':
    print("Using one key, we can infinitely keep the key lifetime going")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinUnlimitedKeyAllocator(num_keys=1),
            elem_consume=FifoElementConsumer(num_elems=256),
        ).run(10_000)
    )

    print("")

    print("Even if we round-robin multiple (e.g. 4) keys, a FIFO queue will make them all last infinitely if they don't have a limit")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinUnlimitedKeyAllocator(num_keys=4),
            elem_consume=FifoElementConsumer(num_elems=256),
        ).run(10_000)
    )

    print("")

    print("Unless the number of elements (4) <= the number of keys (4)")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinUnlimitedKeyAllocator(num_keys=4),
            elem_consume=FifoElementConsumer(num_elems=4),
        ).run(10_000)
    )

    print("")

    print("Even if the number of elements (5) is close to the number of keys (4), they will last infinitely")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinUnlimitedKeyAllocator(num_keys=4),
            elem_consume=FifoElementConsumer(num_elems=5),
        ).run(10_000)
    )

    print("")

    print("with random allocating/consuming, technically your exposure slack is more likely to be good than bad the closer q is to k.\nkeys=4, elems=5")
    rand = random.Random(12398724)
    log_stats(
        SaturatedQueue(
            key_alloc=UniformRandomUnlimitedKeyAllocator(num_keys=4, rand=rand),
            elem_consume=UniformRandomElementConsumer(num_elems=5, rand=rand),
        ).run(10_000)
    )
    print("keys=4, elems=6")
    rand = random.Random(231098)
    log_stats(
        SaturatedQueue(
            key_alloc=UniformRandomUnlimitedKeyAllocator(num_keys=4, rand=rand),
            elem_consume=UniformRandomElementConsumer(num_elems=6, rand=rand),
        ).run(10_000)
    )
    print("but keys=4, elems=16 (not that much greater) is impossible")
    rand = random.Random(231098)
    log_stats(
        SaturatedQueue(
            key_alloc=UniformRandomUnlimitedKeyAllocator(num_keys=4, rand=rand),
            elem_consume=UniformRandomElementConsumer(num_elems=256, rand=rand),
        ).run(10_000)
    )

    print("")

    print("setting a limit is good, the limit should be at least N_ELEMS_PER_QUEUE/N_KEYS_PER_QUEUE")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=256 // 4),
            elem_consume=FifoElementConsumer(num_elems=256)
        ).run(10_000)
    )

    print("")

    print("but that has crap availability! going higher is necessary, but it increases the consume-to-free time")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=(256 // 3)),
            elem_consume=FifoElementConsumer(num_elems=256)
        ).run(10_000)
    )

    # print("")

    # print("going lower lowers the effective maximum queue size. it therefore decreases the consume-to-free time by proxy")
    # log_stats(
    #     SaturatedQueue(
    #         key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=(256 // 4) // 2),
    #         elem_consume=FifoElementConsumer(num_elems=256)
    #     ).run(10_000)
    # )

    print("")

    print("OoO completion plays hell with this")
    rand = random.Random(231098)
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=256 // 4),
            elem_consume=UniformRandomElementConsumer(num_elems=256, rand=rand)
        ).run(10_000)
    )

    print("")

    print("and I thought it would be helped by guaranteeing to only take items from a 'front window' but that was false.")
    print("consider that this scheme would just end up with a whole mix of 1,2,3,4 all at the front, even if it initially started with 1111122222233333444444 - removing random elements in the first quadrant (i.e. all 1s) just lets new elements (i.e. all 2s) seep in")
    rand = random.Random(124343)
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=256 // 4),
            elem_consume=FrontRestrictedUniformRandomElementConsumer(num_elems=256, num_front_elems=64, rand=rand),
        ).run(10_000)
    )

    print("closing that window helps a bit. still bad tho.")
    rand = random.Random(497234)
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=256 // 4),
            elem_consume=FrontRestrictedUniformRandomElementConsumer(num_elems=256, num_front_elems=16, rand=rand),
        ).run(10_000)
    )

    print("16 keys with window=16 helps more. still bad tho.")
    rand = random.Random(3567457)
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=16, key_limit=256 // 16),
            elem_consume=FrontRestrictedUniformRandomElementConsumer(num_elems=256, num_front_elems=16, rand=rand),
        ).run(10_000)
    )

    print("")

    print("the only way to get full availability with less consume-to-free time is to use more keys and complete FIFO")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=9, key_limit=(256 // 8)),
            elem_consume=FifoElementConsumer(num_elems=256)
        ).run(10_000)
    )
    # log_stats(
    #     SaturatedQueue(
    #         key_alloc=RoundRobinLimitKeyAllocator(num_keys=4, key_limit=256 // 4),
    #         elem_consume=FifoElementConsumer(num_elems=256)
    #     ).run(10_000)
    # )

    print("netflix freebsd NVMe sim = 1MiB blocks, 16 queues. keys per queue=9? => 144 keys total.")
    print("For an NVMe our modelling is an upper bound, because the NVMe may do things out-of-order but only to our benefit?")
    print("e.g. if two reads hit in different physical flash elements, they might be done in parallel?")
    print("also, if we aren't reusing this memory for anything other than e.g. NVMe read targets, we don't need to worry about the exposure slack.")
    print("... with FIFO consumer keys x9")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=9, key_limit=(128 // 8)),
            elem_consume=FifoElementConsumer(num_elems=128)
        ).run(10_000),
        elem_size=2 ** 20,
        n_queues=16,
    )

    print("... with FIFO consumer keys x17")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=17, key_limit=(128 // 16)),
            elem_consume=FifoElementConsumer(num_elems=128)
        ).run(10_000),
        elem_size=2 ** 20,
        n_queues=16,
    )

    print("... with FIFO consumer keys x33")
    log_stats(
        SaturatedQueue(
            key_alloc=RoundRobinLimitKeyAllocator(num_keys=33, key_limit=(128 // 32)),
            elem_consume=FifoElementConsumer(num_elems=128)
        ).run(10_000),
        elem_size=2 ** 20,
        n_queues=16,
    )

    for window in [16, 8, 4]:	
        print(f"... and with {window} front OoO window")
        log_stats(
            SaturatedQueue(
                key_alloc=RoundRobinLimitKeyAllocator(num_keys=9, key_limit=(128 // 8)),
                elem_consume=FrontRestrictedUniformRandomElementConsumer(num_elems=128, num_front_elems=window, rand=rand),
            ).run(10_000),
            elem_size=2 ** 20,
            n_queues=16,
        )

    print("TODO - try a more realistic OoO age-based model")


    print("high-perf network card model")
    print("from https://docs.nvidia.com/grace-perf-tuning-guide/optimizing-io.html")
    print("queue sizes = 8192 per RX, 8192 per TX")
    QUEUE_LEN = 8192
    print("max MTU sizes = 9000B")
    print("max TCP TX packet size for split-offload = 64000B (https://docs.nvidia.com/networking/display/winof2v237/configuring+the+driver+registry+keys)")
    print("TODO assume 128 RX, 128 TX quuees?")
    print("TODO remember this should be compared to pre-allocating a pool of (n queues * n work elems per queue * size of work elem)")
    N_RX_QUEUES=N_TX_QUEUES=128 # (??)
    for keys in [4+1, 8+1, 16+1, 32+1, 64+1, 128+1]:
        print(f"... with {keys} keys per queue, {keys*(N_RX_QUEUES+N_TX_QUEUES)} total (2^{math.ceil(math.log(keys*(N_RX_QUEUES+N_TX_QUEUES), 2))})")
        log_stats(
            SaturatedQueue(
                key_alloc=RoundRobinLimitKeyAllocator(num_keys=keys, key_limit=(QUEUE_LEN // (keys - 1))),
                elem_consume=FifoElementConsumer(num_elems=QUEUE_LEN)
            ).run(10_000),
            elem_size=64000,
            n_queues=N_RX_QUEUES + N_TX_QUEUES,
        )

    print("low-perf network card model")
    print("assume max TCP packet size = max MTU = 1500 on my nmachine. pad up to 4KiB")
    # https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-net-queues
    print("tx queue limit_max = 1879048192? which would imply 1 million packets, but that's the absolute maximum limit of bytes allowed to be queued on this network device transmit queue")
    print("tx queue limit rn = 4482 = 4 packets. this is dynamic. it jumped to 6002 (6ish packets?)")
    print("one rx queue, one tx queue")
    QUEUE_LEN = 16
    N_RX_QUEUES=N_TX_QUEUES=1
    for keys in [4+1, 8+1]:
        print(f"... with {keys} keys per queue, {keys*(N_RX_QUEUES+N_TX_QUEUES)} total (2^{math.ceil(math.log(keys*(N_RX_QUEUES+N_TX_QUEUES), 2))})")
        log_stats(
            SaturatedQueue(
                key_alloc=RoundRobinLimitKeyAllocator(num_keys=keys, key_limit=(QUEUE_LEN // (keys - 1))),
                elem_consume=FifoElementConsumer(num_elems=QUEUE_LEN)
            ).run(10_000),
            elem_size=4096,
            n_queues=N_RX_QUEUES + N_TX_QUEUES,
        )
    print("with a slightly more sensible queue len of 128?")
    QUEUE_LEN = 128
    N_RX_QUEUES=N_TX_QUEUES=1
    for keys in [4+1, 8+1]:
        print(f"... with {keys} keys per queue, {keys*(N_RX_QUEUES+N_TX_QUEUES)} total (2^{math.ceil(math.log(keys*(N_RX_QUEUES+N_TX_QUEUES), 2))})")
        log_stats(
            SaturatedQueue(
                key_alloc=RoundRobinLimitKeyAllocator(num_keys=keys, key_limit=(QUEUE_LEN // (keys - 1))),
                elem_consume=FifoElementConsumer(num_elems=QUEUE_LEN)
            ).run(10_000),
            elem_size=4096,
            n_queues=N_RX_QUEUES + N_TX_QUEUES,
        )
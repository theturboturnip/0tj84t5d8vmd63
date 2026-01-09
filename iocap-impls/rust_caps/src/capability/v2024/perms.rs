use crate::capability::{CapDecodeErr, CapEncodeErr, CapPerms};


// These are non-camel-case for a reason
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapPermsChain {
    /// The initial resource is read-only, and no caveats are active
    ///
    /// `[Read]`
    ReadOnly_0Cav,
    /// The initial resource is read-only, and the first caveat is active
    ///
    /// `[Read, Read]`
    ReadOnly_1Cav,
    /// The initial resource is read-only, and both caveats are active
    ///
    /// `[Read, Read, Read]`
    ReadOnly_2Cav,
    /// The initial resource is wrute-only, and no caveats are active
    ///
    /// `[Write]`
    WriteOnly_0Cav,
    /// The initial resource is write-only, and the first caveat is active
    ///
    /// `[Write, Write]`
    WriteOnly_1Cav,
    /// The initial resource is write-only, and both caveats are active
    ///
    /// `[Write, Write, Write]`
    WriteOnly_2Cav,
    /// The initial resource is read-write, and no caveats are active
    ///
    /// `[ReadWrite]`
    ReadWrite_0Cav,
    /// The initial resource is read-write, the first caveat is active and didn't shrink the permissions
    ///
    /// `[ReadWrite, ReadWrite]`
    ReadWrite_1Cav,
    /// The initial resource is read-write, both caveats are active and neither shrunk the permissions
    ///
    /// `[ReadWrite, ReadWrite, ReadWrite]`
    ReadWrite_2Cav,
    /// The initial resource was read-write, the first caveat is active and shrunk the permissions to read-only
    ///
    /// `[ReadWrite, Read]`
    ShrunkToRead_1Cav,
    /// The initial resource was read-write, the first caveat is active and shrunk the permissions to write-only
    ///
    /// `[ReadWrite, Write]`
    ShrunkToWrite_1Cav,
    /// The initial resource was read-write, both caveats are active and the first caveat shrunk the permissions to read-only
    ///
    /// `[ReadWrite, Read, Read]`
    ShrunkToRead_2Cav_ByCav1,
    /// The initial resource was read-write, both caveats are active and the second caveat shrunk the permissions to read-only
    ///
    /// `[ReadWrite, ReadWrite, Read]`
    ShrunkToRead_2Cav_ByCav2,
    /// The initial resource was read-write, both caveats are active and the first caveat shrunk the permissions to write-only
    ///
    /// `[ReadWrite, Write, Write]`
    ShrunkToWrite_2Cav_ByCav1,
    /// The initial resource was read-write, both caveats are active and the second caveat shrunk the permissions to write-only
    ///
    /// `[ReadWrite, ReadWrite, Write]`
    ShrunkToWrite_2Cav_ByCav2,
}

impl CapPermsChain {
    pub fn n_cavs(self) -> u8 {
        use CapPermsChain::*;
        match self {
            ReadOnly_0Cav | WriteOnly_0Cav | ReadWrite_0Cav => 0,
            ReadOnly_1Cav | WriteOnly_1Cav | ReadWrite_1Cav | ShrunkToRead_1Cav | ShrunkToWrite_1Cav => 1,
            ReadOnly_2Cav | WriteOnly_2Cav | ReadWrite_2Cav | ShrunkToRead_2Cav_ByCav1 | ShrunkToRead_2Cav_ByCav2 | ShrunkToWrite_2Cav_ByCav1 | ShrunkToWrite_2Cav_ByCav2 => 2,
        }
    }
    pub fn root(self) -> Self {
        use CapPermsChain::*;
        match self {
            ReadOnly_0Cav | ReadOnly_1Cav | ReadOnly_2Cav => ReadOnly_0Cav,
            WriteOnly_0Cav | WriteOnly_1Cav | WriteOnly_2Cav => WriteOnly_0Cav,
            ReadWrite_0Cav | ReadWrite_1Cav | ReadWrite_2Cav | ShrunkToRead_1Cav | ShrunkToWrite_1Cav | ShrunkToRead_2Cav_ByCav1 | ShrunkToRead_2Cav_ByCav2 | ShrunkToWrite_2Cav_ByCav1 | ShrunkToWrite_2Cav_ByCav2 => ReadWrite_0Cav,
        }
    }
    pub fn at_cav_1(self) -> Option<Self> {
        use CapPermsChain::*;
        match self {
            // 0-caveat chains don't have a cav1
            ReadOnly_0Cav | WriteOnly_0Cav | ReadWrite_0Cav=> None,
            // 1-caveat chains *are* their cav1
            ReadOnly_1Cav => Some(self),
            WriteOnly_1Cav => Some(self),
            ReadWrite_1Cav => Some(self),
            ShrunkToRead_1Cav => Some(self),
            ShrunkToWrite_1Cav => Some(self),
            // 2-caveat chains are based on a cav1
            ReadOnly_2Cav => Some(ReadOnly_1Cav),
            WriteOnly_2Cav => Some(WriteOnly_1Cav),
            ReadWrite_2Cav => Some(ReadWrite_1Cav),
            ShrunkToRead_2Cav_ByCav1 => Some(ShrunkToRead_1Cav),
            ShrunkToRead_2Cav_ByCav2 => Some(ReadWrite_1Cav),
            ShrunkToWrite_2Cav_ByCav1 => Some(ShrunkToWrite_1Cav),
            ShrunkToWrite_2Cav_ByCav2 => Some(ReadWrite_1Cav),
        }
    }

    pub fn at_cav_2(self) -> Option<Self> {
        use CapPermsChain::*;
        match self {
            // 0-caveat chains don't have a cav2
            ReadOnly_0Cav | WriteOnly_0Cav | ReadWrite_0Cav => None,
            // 1-caveat chains don't have a cav2
            ReadOnly_1Cav | WriteOnly_1Cav | ReadWrite_1Cav | ShrunkToRead_1Cav | ShrunkToWrite_1Cav => None,
            // 2-caveat chains *are* their cav2
            ReadOnly_2Cav => Some(self),
            WriteOnly_2Cav => Some(self),
            ReadWrite_2Cav => Some(self),
            ShrunkToRead_2Cav_ByCav1 => Some(self),
            ShrunkToRead_2Cav_ByCav2 => Some(self),
            ShrunkToWrite_2Cav_ByCav1 => Some(self),
            ShrunkToWrite_2Cav_ByCav2 => Some(self),
        }
    }


    pub fn back_in_time(self) -> Option<Self> {
        use CapPermsChain::*;
        let b = match self {
            ReadOnly_0Cav | WriteOnly_0Cav | ReadWrite_0Cav => return None,
            ReadOnly_1Cav => ReadOnly_0Cav,
            ReadOnly_2Cav => ReadOnly_1Cav,
            WriteOnly_1Cav => WriteOnly_0Cav,
            WriteOnly_2Cav => WriteOnly_1Cav,
            ReadWrite_1Cav => ReadWrite_0Cav,
            ReadWrite_2Cav => ReadWrite_1Cav,
            ShrunkToRead_1Cav => ReadWrite_0Cav,
            ShrunkToWrite_1Cav => ReadWrite_0Cav,
            ShrunkToRead_2Cav_ByCav1 => ShrunkToRead_1Cav,
            ShrunkToRead_2Cav_ByCav2 => ReadWrite_1Cav,
            ShrunkToWrite_2Cav_ByCav1 => ShrunkToWrite_1Cav,
            ShrunkToWrite_2Cav_ByCav2 => ReadWrite_1Cav,
        };
        Some(b)
    }
    pub fn try_add_caveat(self, new_perms: CapPerms) -> Result<Self, CapEncodeErr> {
        use CapPermsChain::*;
        match (self, new_perms) {
            (ReadOnly_2Cav | WriteOnly_2Cav | ReadWrite_2Cav | ShrunkToRead_2Cav_ByCav1 | ShrunkToRead_2Cav_ByCav2 | ShrunkToWrite_2Cav_ByCav1 | ShrunkToWrite_2Cav_ByCav2, _)=> Err(CapEncodeErr::NoCaveatsLeft),

            (ReadWrite_0Cav, CapPerms::ReadWrite) => Ok(ReadWrite_1Cav),
            (ReadWrite_0Cav, CapPerms::Read) => Ok(ShrunkToRead_1Cav),
            (ReadWrite_0Cav, CapPerms::Write) => Ok(ShrunkToWrite_1Cav),
            (ReadWrite_1Cav, CapPerms::ReadWrite) => Ok(ReadWrite_2Cav),
            (ReadWrite_1Cav, CapPerms::Read) => Ok(ShrunkToRead_2Cav_ByCav2),
            (ReadWrite_1Cav, CapPerms::Write) => Ok(ShrunkToWrite_2Cav_ByCav2),

            (ReadOnly_0Cav, CapPerms::Read) => Ok(ReadOnly_1Cav),
            (ReadOnly_1Cav, CapPerms::Read) => Ok(ReadOnly_2Cav),
            (ShrunkToRead_1Cav, CapPerms::Read) => Ok(ShrunkToRead_2Cav_ByCav1),
            (ReadOnly_0Cav | ReadOnly_1Cav | ShrunkToRead_1Cav, _) => Err(CapEncodeErr::CantShrinkPerms(new_perms)),

            (WriteOnly_0Cav, CapPerms::Write) => Ok(WriteOnly_1Cav),
            (WriteOnly_1Cav, CapPerms::Write) => Ok(WriteOnly_2Cav),
            (ShrunkToWrite_1Cav, CapPerms::Write) => Ok(ShrunkToWrite_2Cav_ByCav1),
            (WriteOnly_0Cav | WriteOnly_1Cav | ShrunkToWrite_1Cav, _) => Err(CapEncodeErr::CantShrinkPerms(new_perms)),
        }
    }
    pub fn from_root(perms: CapPerms) -> CapPermsChain {
        match perms {
            CapPerms::Read => CapPermsChain::ReadOnly_0Cav,
            CapPerms::Write => CapPermsChain::WriteOnly_0Cav,
            CapPerms::ReadWrite => CapPermsChain::ReadWrite_0Cav,
        }
    }
    pub fn encode(self) -> u8 {
        self.into()
    }
}

impl From<CapPermsChain> for u8 {
    fn from(value: CapPermsChain) -> Self {
        use CapPermsChain::*;
        match value {
            // if 0bXX_11 then it's read/write, XX = num caveats
            ReadWrite_0Cav => 0b00_11,
            ReadWrite_1Cav => 0b01_11,
            ReadWrite_2Cav => 0b10_11,
            // 0b1111 not used

            // if 0b00_XX where XX != 11 then it's read only, no reductions, XX = num caveats
            ReadOnly_0Cav => 0b00_00,
            ReadOnly_1Cav => 0b00_01,
            ReadOnly_2Cav => 0b00_10,

            // if 0b10_10 then shrunk to read by first and only caveat
            ShrunkToRead_1Cav => 0b10_10,
            // if 0b10_0X then 2 caveats, shrunk to read by caveat #(X+1)
            ShrunkToRead_2Cav_ByCav1 => 0b10_00,
            ShrunkToRead_2Cav_ByCav2 => 0b10_01,

            // if 0b11_XX where XX != 11 then it's write only, no reductions, XX caveats
            WriteOnly_0Cav => 0b11_00,
            WriteOnly_1Cav => 0b11_01,
            WriteOnly_2Cav => 0b11_10,

            // if 0b01_10 then shrunk to write by first and only caveat
            ShrunkToWrite_1Cav => 0b01_10,
            // if 0b01_0X then 2 caveats, shrunk to write by caveat #(X+1)
            ShrunkToWrite_2Cav_ByCav1 => 0b01_00,
            ShrunkToWrite_2Cav_ByCav2 => 0b01_01,
        }
    }
}
impl TryFrom<u8> for CapPermsChain {
    type Error = CapDecodeErr;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use CapPermsChain::*;
        let c = match value {
            0b0000 => ReadOnly_0Cav,
            0b0001 => ReadOnly_1Cav,
            0b0010 => ReadOnly_2Cav,
            0b0011 => ReadWrite_0Cav,

            0b0100 => ShrunkToWrite_2Cav_ByCav1,
            0b0101 => ShrunkToWrite_2Cav_ByCav2,
            0b0110 => ShrunkToWrite_1Cav,
            0b0111 => ReadWrite_1Cav,

            0b1000 => ShrunkToRead_2Cav_ByCav1,
            0b1001 => ShrunkToRead_2Cav_ByCav2,
            0b1010 => ShrunkToRead_1Cav,
            0b1011 => ReadWrite_2Cav,

            0b1100 => WriteOnly_0Cav,
            0b1101 => WriteOnly_1Cav,
            0b1110 => WriteOnly_2Cav,
            0b1111 | _ => return Err(CapDecodeErr::InvalidCapPermsChain),
        };
        Ok(c)
    }
}

impl From<CapPermsChain> for CapPerms {
    fn from(value: CapPermsChain) -> Self {
        use CapPermsChain::*;
        match value {
            ReadWrite_0Cav | ReadWrite_1Cav | ReadWrite_2Cav => CapPerms::ReadWrite,
            ReadOnly_0Cav | ReadOnly_1Cav | ReadOnly_2Cav | ShrunkToRead_1Cav | ShrunkToRead_2Cav_ByCav1 | ShrunkToRead_2Cav_ByCav2 => CapPerms::Read,
            WriteOnly_0Cav | WriteOnly_1Cav | WriteOnly_2Cav | ShrunkToWrite_1Cav | ShrunkToWrite_2Cav_ByCav1 | ShrunkToWrite_2Cav_ByCav2 => CapPerms::Write
        }
    }
}

pub const ALL_0CAV_CAP_PERMS_CHAINS: [CapPermsChain; 3] = [
    CapPermsChain::ReadOnly_0Cav,
    CapPermsChain::WriteOnly_0Cav,
    CapPermsChain::ReadWrite_0Cav,
];

pub const ALL_1CAV_CAP_PERMS_CHAINS: [CapPermsChain; 5] = [
    CapPermsChain::ReadOnly_1Cav,
    CapPermsChain::WriteOnly_1Cav,
    CapPermsChain::ReadWrite_1Cav,
    CapPermsChain::ShrunkToRead_1Cav,
    CapPermsChain::ShrunkToWrite_1Cav,
];

pub const ALL_0_OR_1CAV_CAP_PERMS_CHAINS: [CapPermsChain; 8] = [
    CapPermsChain::ReadOnly_0Cav,
    CapPermsChain::WriteOnly_0Cav,
    CapPermsChain::ReadWrite_0Cav,

    CapPermsChain::ReadOnly_1Cav,
    CapPermsChain::WriteOnly_1Cav,
    CapPermsChain::ReadWrite_1Cav,
    CapPermsChain::ShrunkToRead_1Cav,
    CapPermsChain::ShrunkToWrite_1Cav,
];

pub const ALL_2CAV_CAP_PERMS_CHAINS: [CapPermsChain; 7] = [
    CapPermsChain::ReadOnly_2Cav,
    CapPermsChain::WriteOnly_2Cav,
    CapPermsChain::ReadWrite_2Cav,
    CapPermsChain::ShrunkToRead_2Cav_ByCav1,
    CapPermsChain::ShrunkToRead_2Cav_ByCav2,
    CapPermsChain::ShrunkToWrite_2Cav_ByCav1,
    CapPermsChain::ShrunkToWrite_2Cav_ByCav2,
];

pub const ALL_VALID_CAP_PERMS_CHAINS: [CapPermsChain; 15] = [
    CapPermsChain::ReadOnly_0Cav,
    CapPermsChain::ReadOnly_1Cav,
    CapPermsChain::ReadOnly_2Cav,

    CapPermsChain::WriteOnly_0Cav,
    CapPermsChain::WriteOnly_1Cav,
    CapPermsChain::WriteOnly_2Cav,

    CapPermsChain::ReadWrite_0Cav,
    CapPermsChain::ReadWrite_1Cav,
    CapPermsChain::ReadWrite_2Cav,

    CapPermsChain::ShrunkToRead_1Cav,
    CapPermsChain::ShrunkToRead_2Cav_ByCav1,
    CapPermsChain::ShrunkToRead_2Cav_ByCav2,


    CapPermsChain::ShrunkToWrite_1Cav,
    CapPermsChain::ShrunkToWrite_2Cav_ByCav1,
    CapPermsChain::ShrunkToWrite_2Cav_ByCav2,
];


#[cfg(test)]
mod test {
    use super::{CapPermsChain, ALL_VALID_CAP_PERMS_CHAINS};

    #[test]
    pub fn back_and_forth() {
        let to_test = ALL_VALID_CAP_PERMS_CHAINS;

        for t in to_test {
            let t_as_u8: u8 = t.into();
            let t_after_reconvert: CapPermsChain = t_as_u8.try_into().unwrap();
        
            assert_eq!(t, t_after_reconvert)
        }
    }

    // TODO more complex chain traversal tests
}
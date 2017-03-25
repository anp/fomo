use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use glob::Pattern;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::{Unexpected, Visitor};

#[derive(Clone, Debug)]
pub struct Globber(pub Pattern);

impl Serialize for Globber {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    serializer.serialize_str(self.as_str())
  }
}

impl Deserialize for Globber {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer
  {
    struct VariantVisitor<__D> {
      _marker: PhantomData<__D>,
    }

    impl<__D> Visitor for VariantVisitor<__D>
      where __D: Deserializer
    {
      type Value = Globber;

      fn visit_str<E>(self, value: &str) -> Result<Globber, E>
        where E: ::serde::de::Error
      {
        Globber::from_str(value).map_err(|_| E::invalid_value(Unexpected::Str(value), &self))
      }

      fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        formatter.write_str("a valid file path glob expression")
      }
    }

    deserializer.deserialize_str(VariantVisitor::<D> { _marker: PhantomData, })
  }
}

impl FromStr for Globber {
  type Err = <Pattern as FromStr>::Err;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Pattern::from_str(s).map(|p| Globber(p))
  }
}

impl Deref for Globber {
  type Target = Pattern;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

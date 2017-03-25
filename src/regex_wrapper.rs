use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::{Unexpected, Visitor};

#[derive(Clone, Debug)]
pub struct RegexMatcher(pub Regex);

impl Serialize for RegexMatcher {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    serializer.serialize_str(self.as_str())
  }
}

impl Deserialize for RegexMatcher {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer
  {
    struct VariantVisitor<__D> {
      _marker: PhantomData<__D>,
    }

    impl<__D> Visitor for VariantVisitor<__D>
      where __D: Deserializer
    {
      type Value = RegexMatcher;

      fn visit_str<E>(self, value: &str) -> Result<RegexMatcher, E>
        where E: ::serde::de::Error
      {
        RegexMatcher::from_str(value).map_err(|_| E::invalid_value(Unexpected::Str(value), &self))
      }

      fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        formatter.write_str("a valid file path glob expression")
      }
    }

    deserializer.deserialize_str(VariantVisitor::<D> { _marker: PhantomData, })
  }
}

impl FromStr for RegexMatcher {
  type Err = <Regex as FromStr>::Err;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Regex::from_str(s).map(|p| RegexMatcher(p))
  }
}

impl Deref for RegexMatcher {
  type Target = Regex;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

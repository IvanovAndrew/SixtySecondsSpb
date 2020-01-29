module Config

open FSharp.Data

type Config = XmlProvider<"app-config.xml">
void foo()
{
  version (a)
  {
  }
  else version (b)
  {
  }
  else
  {
  }

  debug (A)
  {
  }
  else debug (B)
  {
  }
  else
  {
  }

  version (a)
  {
  }
  else
    version (b)
    {
    }
    else
    {
    }

  if (true)
  {
  }
  else
    if (true)
    {
    }
    else
    {
    }

  static if (1 < 2)
  {
  }
  else static if (false)
  {
  }
  else static if (true)
  {
  }
}

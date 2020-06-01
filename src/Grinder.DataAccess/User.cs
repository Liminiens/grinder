using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Grinder.DataAccess
{
  public class User
  {
    [Required]
    public long UserId { get; set; }

    [Required]
    public string Username { get; set; }
  }
}
using System.ComponentModel.DataAnnotations;
using System;

namespace Grinder.DataAccess
{
  public class Message
  {
    [Required]
    public long MessageId { get; set; }

    [Required]
    public long ChatId { get; set; }

    [Required]
    public long UserId { get; set; }

    [Required]
    public long Date { get; set; } = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
  }
}
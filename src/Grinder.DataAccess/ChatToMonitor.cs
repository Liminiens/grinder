using System.ComponentModel.DataAnnotations;

namespace Grinder.DataAccess
{
  public class ChatToMonitor
  {
    [Key]
    [Required]
    public string Username { get; set; }
  }
}
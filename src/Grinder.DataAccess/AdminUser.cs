using System.ComponentModel.DataAnnotations;

namespace Grinder.DataAccess
{
  public class AdminUser
  {
    [Key]
    [Required]
    public string Username { get; set; }
  }
}
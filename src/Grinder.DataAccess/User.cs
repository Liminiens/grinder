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

    public class Message
    {
        [Required]
        public long MessageId { get; set; }

        [Required]
        public long ChatId { get; set; }
    
        [Required]
        public long UserId { get; set; }
    }
}